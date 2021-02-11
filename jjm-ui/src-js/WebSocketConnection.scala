package jjm.ui

import org.scalajs.dom.WebSocket
import org.scalajs.dom.raw.CloseEvent
import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.raw.Event

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

class WebSocketConnection[Request, Response](
  requestToString: Request => String,
  responseFromString: String => Response) {

  sealed trait State {
    def toContext = this match {
      case ConnectingState => Connecting
      case ConnectedState(socket) => Connected((r: Request) => Callback(socket.send(requestToString(r))))
    }
  }
  case object ConnectingState extends State
  case class ConnectedState(socket: WebSocket) extends State

  case class Props(
    websocketURI: String,
    onMessage: (((Request => Callback), Response) => Callback) = ((_, _) => Callback.empty),
    render: Context => VdomElement
  )

  sealed trait Context
  case object Connecting extends Context
  case class Connected(send: Request => Callback) extends Context

  class Backend(scope: BackendScope[Props, State]) {

    def connect(props: Props): Callback = scope.state map {
      case ConnectingState =>
        val socket = new WebSocket(props.websocketURI)
        val send = (r: Request) => Callback(socket.send(requestToString(r)))
        socket.onopen = { (event: Event) =>
          scope.setState(ConnectedState(socket)).runNow()
        }
        socket.onerror = { (event: Event) =>
          val msg = s"Connection failure. Error: $event"
          System.err.println(msg)
        }
        socket.onmessage = { (event: MessageEvent) =>
          props.onMessage(send, responseFromString(event.data.toString)).runNow()
        }
        socket.onclose = { (event: CloseEvent) =>
          val cleanly = if (event.wasClean) "cleanly" else "uncleanly"
          val msg = s"WebSocket connection closed $cleanly with code ${event.code}. reason: ${event.reason}"
          System.err.println(msg)
        }
      case ConnectedState(_) =>
        System.err.println("Already connected.")
    }

    def close(s: State): Callback = s match {
      case ConnectingState => Callback.empty
      case ConnectedState(socket) => Callback(socket.close(1000))
    }

    def render(props: Props, s: State) =
      props.render(s.toContext)
  }

  val WebSocket = ScalaComponent
    .builder[Props]("WebSocket")
    .initialState(ConnectingState: State)
    .renderBackend[Backend]
    .componentDidMount(context => context.backend.connect(context.props))
    .componentWillUnmount(context => context.backend.close(context.state))
    .build
}
