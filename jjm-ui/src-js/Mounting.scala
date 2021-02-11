package jjm.ui

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

object Mounting {
  case class Props(
    callback: Callback,
    render: VdomElement
  )

  val Component = ScalaComponent
    .builder[Props]("Mounting")
    .render { $ => $.props.render }
    .componentDidMount(_.props.callback)
    .build

  def make(callback: Callback)(
    render: VdomElement
  ) = {
    Component(Props(callback, render))
  }
}
