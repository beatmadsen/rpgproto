package com.madsen.gameproto.akka

import java.net.InetSocketAddress

import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.madsen.gameproto.akka.UpdateProtocol.{UpdateComplete, _}
import com.madsen.gameproto.model.LagTracker
import play.api.libs.json._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.io.StdIn

/**
 * Created by erikmadsen on 11/10/2015.
 */
object GameApp extends App {

  val system = ActorSystem("rpg-proto-system")

  // system.actorOf(Props[GameLoop], "game-loop")
  system.actorOf(Props[EchoServer], "echo-server")

  println("Node started. Kill with 'q' + enter")
  Stream.continually(StdIn.readLine()).takeWhile(_ != "q")

  system.shutdown()
}


class GameLoop extends Actor {

  import scala.concurrent.duration._

  implicit val ec: ExecutionContextExecutor = context.dispatcher
  implicit val timeout: Timeout = Timeout(1.minute)

  val um: ActorRef = context.actorOf(Props[UpdateManager], "update-manager")


  override def receive: Receive = {

    case NextTurn ⇒ runOneTurn()
  }


  self ! NextTurn


  private def runOneTurn(): Unit = {


    // line up user requested changes to game state

    val steps: List[Future[Unit]] = processInput() ::
      update() ::
      render() ::
      Nil

    val f: Future[List[Unit]] = Future.sequence(steps)

    f foreach { _ ⇒
      self ! NextTurn
    }
  }


  private def update(): Future[Unit] = {

    (um ? UpdateInit(self)) map (_ ⇒ ())
  }


  private def render(): Future[Unit] = ???


  private def processInput(): Future[Unit] = ???


  case object NextTurn


}


object UpdateProtocol {


  case class UpdateInit(completeTo: ActorRef)


  case object UpdateComplete


}


/**
 * Singleton that manages updates vs lag
 */
class UpdateManager extends Actor {


  val UpdateFps: Int = 60
  val MillisPerUpdate: Double = 1000.0 / UpdateFps
  val lagTracker = new LagTracker

  implicit val ec: ExecutionContextExecutor = context.dispatcher


  override def receive: Actor.Receive = {

    case UpdateInit(completeTo) ⇒

      lagTracker.tick()

      self ! UpdateCarryOutWork(completeTo)

    case UpdateCarryOutWork(completeTo) ⇒

      if (lagTracker.latestLag >= MillisPerUpdate) {
        // do work
        doWork() foreach { _ ⇒
          lagTracker.commitWork(MillisPerUpdate)
          self ! UpdateCarryOutWork(completeTo) // loop
        }
      } else {
        completeTo ! UpdateComplete
      }
  }


  private def doWork(): Future[Unit] = {

    // TODO: Delegate work to other actors
    Future {
      1 to 1000 foreach println
    }
  }


  case class UpdateCarryOutWork(completeTo: ActorRef)


}


class EchoServer extends Actor {

  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 9000))


  override def receive: Receive = {

    case b @ Bound(localAddress) ⇒ println(s"bound at $localAddress")

    case CommandFailed(_: Bind) ⇒ context stop self

    case c @ Connected(remote, local) ⇒
      val handler: ActorRef = context actorOf Props[TcpEcho]
      val connection: ActorRef = sender()

      connection ! Register(handler)
  }
}


class TcpEcho extends Actor {

  val Separator: Seq[Byte] = ByteString("\r\n").toArray.toSeq
  var count = 0
  var buffer: ByteString = ByteString.empty


  override def receive: Receive = {

    case Received(data) ⇒ onReceive(data)
    case PeerClosed ⇒ context stop self
  }


  private def onReceive(data: ByteString): Unit = {

    buffer ++= data

    val indexOfSlice: Int = buffer.indexOfSlice(Separator)

    if (indexOfSlice == -1) ()
    else {

      val (_, b) = buffer.splitAt(indexOfSlice + Separator.size)
      buffer = b


      val jsonIn = Json.parse(data.toArray)
      val mRequest: Option[RpgRequest] = jsonIn.asOpt[RpgRequest]

      mRequest foreach {

        case RpgRequest("plus", args) ⇒
          args.get("value") foreach { value ⇒
            count += value.toInt
          }

        case RpgRequest("minus", args) ⇒
          args.get("value") foreach { value ⇒
            count -= value.toInt
          }

        case RpgRequest("get", _) ⇒

          val response: RpgResponse = RpgResponse(count)
          val jsonOut = Json.toJson(response)

          sender() ! Write(ByteString(jsonOut.toString()))

        case _ ⇒ sender ! Write(ByteString("Unexpected input"))
      }
    }
  }
}


object RpgRequest {

  implicit val rpgRequestFormat: Format[RpgRequest] = Json.format[RpgRequest]
}


case class RpgRequest(
  command: String,
  arguments: Map[String, String]
)


object RpgResponse {

  implicit val rpgResponseFormat: Format[RpgResponse] = Json.format[RpgResponse]
}


case class RpgResponse(
  count: Int
)

