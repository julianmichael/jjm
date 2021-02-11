// package jjm.ui

// import spacro.tasks._

// import scalajs.js
// import org.scalajs.dom
// import org.scalajs.dom.WebSocket
// import org.scalajs.dom.raw._
// import org.scalajs.jquery.jQuery

// import scala.concurrent.ExecutionContext.Implicits.global

// import japgolly.scalajs.react.vdom.html_<^._
// import japgolly.scalajs.react._

// import scalacss.DevDefaults._
// import scalacss.ScalaCssReact._

// // import monocle._
// // import monocle.macros._
// // import japgolly.scalajs.react.MonocleReact._

// // import upickle.default._

// object WebSocketConnector {
//   def basicConnector[Request, Response](
//     uri: String
//     writeRequest: Request => String,
//     readResponse: String => Try[Response],
//     receive: Response => Callback
//   ) = {
//     val connect = () => {
//       val promisedConnection = Promise[WebSocket]
//       val ws = new WebSocket(websocketUri)
//       ws.onopen = (_ => promisedConnection.complete(ws))
//       ws.onmessage = ((e: MessageEvent) =>
//         readResponse(e.data.toString) match {
//           case Success(r) => receive(r).runNow
//           case Failure(t) => System.err.println(s"Failed to decode websocket message from $uri - content: ${e.data}")
//         }
//       )
//     }
//   }
// }

// class PushConnection[Connection, Request] {

//   sealed trait State
//   case object Connecting extends State
//   case class Connected(connection: Connection) extends State

//   case class Props(
//     connect: () => Future[Connection],
//     send: (Connection, Request) => Callback,
//     close: Connection => Callback,
//     render: (Request => Callback) => VdomElement
//   )

//   // case class Props(
//   //   websocketURI: String,
//   //   onMessage: (Response => Callback) = (_ => Callback.empty),
//   //   render: (WebSocketState => VdomElement)
//   // )

//   class WebSocketBackend(scope: BackendScope[Props, State]) {

//     def connect(props: Props): Callback = scope.state map {
//       case Connecting =>
//         Callback.future(
//           props.connect().onSuccess { connection =>
//             scope.setState(Connected(connection))
//           }
//         )
//         // val socket = new WebSocket(props.websocketURI)
//         // socket.onopen = { (event: Event) =>
//         //   scope
//         //     .setState(
//         //       Connected(
//         //         (r: Request) =>
//         //           Callback(
//         //             socket.send(write[HeartbeatingWebSocketMessage[Request]](WebSocketMessage(r)))
//         //         )
//         //       )
//         //     )
//         //     .runNow
//         // }
//         socket.onerror = { (event: Event) =>
//           val msg = s"Connection failure. Error: $event"
//           System.err.println(msg)
//         }
//         socket.onmessage = { (event: MessageEvent) =>
//           val msg = event.data.toString
//           read[HeartbeatingWebSocketMessage[Response]](msg) match {
//             case Heartbeat                  => socket.send(msg)
//             case WebSocketMessage(response) => props.onMessage(response).runNow
//           }
//         }
//         socket.onclose = { (event: CloseEvent) =>
//           val cleanly = if (event.wasClean) "cleanly" else "uncleanly"
//           val msg =
//             s"WebSocket connection closed $cleanly with code ${event.code}. reason: ${event.reason}"
//           System.err.println(msg)
//         }
//       case Connected(_) =>
//         System.err.println("Already connected.")
//     }

//     def render(props: WebSocketProps, s: WebSocketState) =
//       props.render(s)
//   }

//   val WebSocket = ScalaComponent
//     .builder[WebSocketProps]("WebSocket")
//     .initialState(Connecting: WebSocketState)
//     .renderBackend[WebSocketBackend]
//     .componentDidMount(context => context.backend.connect(context.props))
//     .build
// }
