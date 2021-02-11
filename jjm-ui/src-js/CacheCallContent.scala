package jjm.ui

import jjm.OrWrapped

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

/** For many-time loading of content, e.g., via ajax, assuming repeated calls will be cached
  * Request and sendRequest are decoupled because the component is designed to change only
  * when the request changes, since sendRequest functions may be difficult to compare.
  */
class CacheCallContent[Request, Response](
  implicit executionContext: ExecutionContext
) {

  sealed trait State
  case object Loading extends State
  case class Loaded(content: Response) extends State

  case class Props(
    request: Request, // will possibly change
    sendRequest: Request => OrWrapped[AsyncCallback, Response], // expected NOT to change
    willLoad: (Response => Callback) = (_ => Callback.empty), // expected NOT to change
    didLoad: (Response => Callback) = (_ => Callback.empty), // expected NOT to change
    render: (State => VdomElement) // expected NOT to change
  )

  class Backend(scope: BackendScope[Props, State]) {

    def load(props: Props): Callback =
      props.sendRequest(props.request) match {
        case OrWrapped.Pure(response) => scope.setState(Loaded(response))
        case OrWrapped.Wrapped(asyncCallback) =>
          scope.setState(Loading) >> asyncCallback.flatMap { response =>
            (props.willLoad(response) >>
                scope.setState(Loaded(response)) >>
                props.didLoad(response)).asAsyncCallback
          }.toCallback
      }

    def render(props: Props, s: State) =
      props.render(s)
  }

  val Component = ScalaComponent
    .builder[Props]("Cache Call Content")
    .initialState(Loading: State)
    .renderBackend[Backend]
    .componentDidMount(context => context.backend.load(context.props))
    .componentDidUpdate(
      context =>
      // NOTE: this conditional might not be necessary? think it is though
        if (context.prevProps.request == context.currentProps.request) {
          Callback.empty
        } else {
          context.backend.load(context.currentProps)
      }
    )
    .build

  def make(
    request: Request,
    sendRequest: Request => OrWrapped[AsyncCallback, Response],
    willLoad: (Response => Callback) = (_ => Callback.empty),
    didLoad: (Response => Callback) = (_ => Callback.empty)
  )(
    render: (State => VdomElement)
  ) = {
    Component(Props(request, sendRequest, willLoad, didLoad, render))
  }
}
