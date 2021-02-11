package jjm.ui

import jjm.ling.ISpan

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import monocle.Lens
import monocle.Prism

class SpanSelection[Index] {

  sealed trait Status
  case object NoSpan extends Status
  case class Selecting(index: Index, anchor: Int, endpoint: Int) extends Status
  object Selecting {
    // For some reason, macros don't work, and I have to define these members
    // in order for the lenses to actually compile. something is messed up
    private[this] val indexSet: (Index => Selecting => Selecting) =
      (i: Index) => (s: Selecting) => s.copy(index = i)
    private[this] val anchorSet: (Int => Selecting => Selecting) =
      (a: Int) => (s: Selecting) => s.copy(anchor = a)
    private[this] val endpointSet: (Int => Selecting => Selecting) =
      (e: Int) => (s: Selecting) => s.copy(endpoint = e)
    val index = Lens[Selecting, Index](_.index)(i => s => s.copy(index = i))
    val anchor = Lens[Selecting, Int](_.anchor)(a => s => s.copy(anchor = a))
    val endpoint = Lens[Selecting, Int](_.endpoint)(e => s => s.copy(endpoint = e))
  }
  object Status {
    val noSpan = Prism[Status, NoSpan.type](
      s => s match { case NoSpan => Some(NoSpan); case _ => None }
    )(identity)
    val selecting = Prism[Status, Selecting](
      s => s match { case s @ Selecting(_, _, _) => Some(s); case _ => None }
    )(identity)
  }

  case class State(
    spans: Map[Index, List[ISpan]],
    status: Status
  )


  object State {
    def initial = State(Map.empty[Index, List[ISpan]].withDefaultValue(Nil), NoSpan)
  }

  case class Context(
    setSpan: Map[Index, List[ISpan]] => Callback,
    hover: Index => Int => Callback,
    touch: Index => Int => Callback,
    cancel: Callback
  )

  case class Props(
    isEnabled: Boolean,
    enableSpanOverlap: Boolean = false,
    update: State => Callback,
    render: (State, Context) => VdomElement
  )

  class Backend(scope: BackendScope[Props, State]) {

    def setSpan(spans: Map[Index, List[ISpan]]): Callback =
      scope.modState(_.copy(spans = spans))

    // not sure why I can't just sequence the update after a modState call. but it seemed to get stuck on the stale state
    // for some reason during the update even though it was sequenced after.
    private[this] def modStateWithUpdate(
      f: State => State
    ): Callback =
      scope.props >>= { props =>
        if (!props.isEnabled) Callback.empty
        else
          scope.state >>= { state =>
            val newState = f(state)
            if (newState != state) {
              scope.setState(newState) >> props.update(newState)
            } else Callback.empty
          }
      }

    def hover(props: Props)(index: Index)(endpoint: Int) = modStateWithUpdate {
      case State(spans, Selecting(`index`, anchor, _)) =>
        val relevantSpans = if (props.enableSpanOverlap) {
          spans.get(index).getOrElse(Nil)
        } else spans.values.toList.flatten
        val range =
          if (anchor <= endpoint) (anchor to endpoint)
          else (anchor to endpoint by -1)
        val newExtremum = range.takeWhile(i => !relevantSpans.exists(_.contains(i))).last
        State(spans, Selecting(index, anchor, newExtremum))
      case x => x
    }

    def touch(props: Props)(index: Index)(wordIndex: Int): Callback =
      modStateWithUpdate {
        case State(spans, NoSpan) =>
          spans(index).zipWithIndex.find(_._1.contains(wordIndex)).map(_._2) match {
            case None =>
              val relevantSpans = if (props.enableSpanOverlap) {
                spans.get(index).getOrElse(Nil)
              } else spans.values.toList.flatten
              if (relevantSpans.exists(_.contains(wordIndex)))
                State(spans, NoSpan) // do nothing
              else
                State(spans, Selecting(index, wordIndex, wordIndex)) // start highlighting
            case Some(i) => // remove span
              State(
                spans.updated(index, spans(index).take(i) ++ spans(index).drop(i + 1)),
                NoSpan
              )
          }
        case State(spans, Selecting(`index`, x, y)) =>
          val relevantSpans = if (props.enableSpanOverlap) {
            spans.get(index).getOrElse(Nil)
          } else spans.values.toList.flatten
          if (relevantSpans.exists(_.contains(wordIndex)))
            State(spans, Selecting(index, x, y)) // do nothing
          else
            State(spans.updated(index, ISpan(x, y) :: spans(index)), NoSpan) // finish span
        case x => x
      }

    def cancel = modStateWithUpdate(_.copy(status = NoSpan))

    def render(props: Props, state: State) =
      props.render(state, Context(setSpan, hover(props), touch(props), cancel))
  }

  val Component = ScalaComponent
    .builder[Props]("Span Selection")
    .initialState(State.initial)
    .renderBackend[Backend]
    .build

  def make(
    isEnabled: Boolean,
    enableSpanOverlap: Boolean = false,
    update: State => Callback)(
    render: (State, Context) => VdomElement
  ) = {
    Component(Props(isEnabled, enableSpanOverlap, update, render))
  }
}
