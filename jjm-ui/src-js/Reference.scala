package jjm.ui

import scala.scalajs.js

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

// allows you to easily use refs inline in DOM creation, if, for example,
// you need to set the location of some element (e.g., a dropdown menu)
// on the basis of the location of another.
class Reference[A <: vdom.TopNode] {

  case class Props(
    referencedTag: VdomTagOf[A],
    render: (VdomTagOf[A], Option[A]) => VdomTag
  )

  type State = Option[A]

  class Backend(scope: BackendScope[Props, State]) {

    val ref = Ref[A]

    def setRef: Callback = scope.setState(scala.util.Try(ref.unsafeGet()).toOption)
    val setRefFn = () => setRef.runNow()

    def render(props: Props, state: Option[A]) =
      props.render(props.referencedTag.withRef(ref), state)
  }

  val Component = ScalaComponent
    .builder[Props]("Reference")
    .initialState(None: State)
    .renderBackend[Backend]
    .componentDidMount { $ =>
      $.backend.setRef >>
        Callback(
          js.Dynamic.global.window.addEventListener(
            "resize",
            $.backend.setRefFn
          )
        )
    }
    .componentWillUnmount { $ =>
      Callback(
        js.Dynamic.global.window.removeEventListener(
          "resize",
          $.backend.setRefFn
        )
      )
    }
    .componentDidUpdate(_.backend.setRef) // TODO make sure this doesn't infinite loop or anything
    .build

  def make(referencedTag: VdomTagOf[A])(
    render: (VdomTagOf[A], Option[A]) => VdomTag
  ) = {
    Component(Props(referencedTag, render))
  }

}
