package jjm

import cats.Id

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object Memo {

  // This should usually only do single-time execution of the given function.
  // but as of now it doesn't quite work that way in some cases. meh
  def memoizeFuture[A, B](
    f: A => Future[B],
    startingCache: Map[A, B] = Map.empty[A, B]
  ): A => OrWrapped[Future, B] = {
    // for thread-safety, make sure all cache updates are run on a single thread
    import java.util.concurrent.Executors
    implicit val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)
    var futCache = Map.empty[A, Future[B]]
    var cache = startingCache

    (a: A) => {
      cache.get(a).map(OrWrapped.pure[Future](_)).getOrElse {
        futCache.get(a).map(OrWrapped.wrapped(_)).getOrElse {
          val bFut = f(a) // failure case is if two threads get here concurrently, in which case both would run the function. meh
          // populate the future cache before attaching the callback to remove it,
          // and update the value cache before depopulating the future cache,
          // so nothing falls through the cracks.
          futCache = futCache + (a -> bFut)
          bFut.foreach { b =>
            cache = cache + (a -> b)
            futCache = futCache - a
          }
          OrWrapped.wrapped(bFut)
        }
      }
    }
  }

  // see caveats in previous method regarding single-time execution
  def memoizeDotFuture[A <: Dot](
    f: DotKleisli[Future, A],
    startingCache: DotMap[Id, A] = DotMap.empty[Id, A]
  ): DotKleisli[OrWrapped[Future, ?], A] = {
    // for thread-safety, make sure all cache updates are run on a single thread
    import java.util.concurrent.Executors
    implicit val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)
    var futCache = DotMap.empty[Future, A]
    var cache = startingCache
    new DotKleisli[OrWrapped[Future, ?], A] {
      override def apply(a: A) = {
        cache.get(a).map(OrWrapped.pure[Future](_)).getOrElse {
          futCache.get(a).map(OrWrapped.wrapped(_)).getOrElse {
            val bFut = f(a)
            futCache = futCache.put(a)(bFut)
            bFut.foreach { b =>
              cache = cache.put(a)(b)
              futCache = futCache.remove(a)
            }
            OrWrapped.wrapped(bFut)
          }
        }
      }
    }
  }

  // TODO
  // def memoizeDependentFuture[F[_], G[_]](

  // )

  // TODO in io/cats-effect extension
  // def memoizeWithRef[F[_]](
  //   f: A => F[B],
  //   cacheRef: Ref[F, Map[A, B]]
  // ): A => OrWrapped[F, B] = (a: A) =>  {
  //   cacheRef.get.flatMap { cache =>
  //     cache.get(a).map(pure[F](_)).getOrElse {
  //       f(a).flatMap { b =>
  //         cacheRef.update(_ + (a -> b)).as(b)
  //       }
  //     }
  //   }
  // }
}
