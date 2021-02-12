package jjm

import scala.collection.mutable

import cats.implicits._

/** Efficient mutable union-find data structure
  * using union-by-rank and path compression. */
class TarjanUnionFind[A] private (
  cells: mutable.Map[A, TarjanUnionFind.UFCell[A]]
) {
  import TarjanUnionFind._

  def add(a: A): Unit = {
    if(!cells.contains(a)) cells.put(a, new UFCell(a))
  }

  def find(a: A): Option[A] = {
    findCell(a).map(_.value)
  }
  private[this] def findCell(a: A): Option[UFCell[A]] = {
    this.cells.get(a).map(ufA => findCellAux(ufA, Nil))
  }
  private[this] def findCellAux(a: UFCell[A], prevCells: List[UFCell[A]]): UFCell[A] = {
    // path compression
    if(a.parent == a) {
      prevCells.foreach(_.parent = a)
      a
    } else {
      findCellAux(a.parent, a :: prevCells)
    }
  }

  def union(a: A, b: A): Option[A] = {
    (findCell(a), findCell(b)).mapN { (x, y) =>
      // union-by-rank
      if(x.rank < y.rank) {
        x.parent = y
        y.value
      } else if(y.rank < x.rank) {
        y.parent = x
        x.value
      } else {
        x.parent = y
        y.rank = y.rank + 1
        y.value
      }
    }
  }
}
object TarjanUnionFind {
  private class UFCell[A](val value: A) {
    var parent: UFCell[A] = this
    var rank: Int = 0
  }
  def empty[A] = new TarjanUnionFind(mutable.Map.empty[A, UFCell[A]])
}
