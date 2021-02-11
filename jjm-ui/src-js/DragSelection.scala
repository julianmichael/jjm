package jjm.ui

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

class DragSelection[Index] {

  sealed trait Action
  case object DoNothing extends Action
  case object Select extends Action
  case object Deselect extends Action

  case class State(span: Set[Index], action: Action)

  object State {
    def init(is: Set[Index]) = State(is, DoNothing)
  }

  case class Context(
    setSpan: Set[Index] => Callback,
    startSelect: Callback,
    startDeselect: Callback,
    stop: Callback,
    touchElement: Index => Callback
  )

  case class Props(
    isEnabled: Boolean,
    preUpdate: State => State = identity,
    update: State => Callback,
    initial: Set[Index] = Set.empty[Index],
    render: (State, Context) => VdomElement
  )

  class Backend(scope: BackendScope[Props, State]) {

    def setSpan(span: Set[Index]): Callback =
      for {
        p <- scope.props
        s <- scope.state
        _ <- scope.setState(p.preUpdate(s.copy(span = span)))
        _ <- scope.state >>= p.update
      } yield ()

    def touchElement(props: Props)(index: Index): Callback =
      scope.modState(
        {
          case s @ State(span, action) =>
            if (!props.isEnabled) s
            else
              props.preUpdate(
                action match {
                  case DoNothing => s
                  case Select => State(span + index, action)
                  case Deselect     => State(span - index, action)
                }
              )
        }: PartialFunction[State, State]
      ) >> scope.props >>= (p => scope.state >>= p.update)

    def setAction(a: Action): Callback =
      for {
        p <- scope.props
        _ <- scope.modState(((s: State) => s.copy(action = a)) andThen p.preUpdate)
        _ <- scope.state >>= p.update
      } yield ()

    val startSelect: Callback = setAction(Select)
    val startDeselect: Callback = setAction(Deselect)
    val stop: Callback = setAction(DoNothing)

    def render(props: Props, state: State) =
      props.render(
        state,
        Context(setSpan, startSelect, startDeselect, stop, touchElement(props))
      )
  }

  val Component = ScalaComponent
    .builder[Props]("Drag Selection")
    .initialStateFromProps(props => State.init(props.initial))
    .renderBackend[Backend]
    .build
}
