package com.madsen.gameproto.akka

import java.net.InetSocketAddress

import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
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

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  import scala.concurrent.duration._

  implicit val timeout: Timeout = Timeout(1.minute)
  val updateManager: ActorRef = context.actorOf(Props[UpdateManager], "update-manager")


  override def receive: Receive = {

    case NextTurn ⇒ runOneTurn()
  }


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


  self ! NextTurn


  private def update(): Future[Unit] = {

    (updateManager ? UpdateInit(self)) map (_ ⇒ ())
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

  val clientRegistry: ActorRef = context actorOf Props[ClientRegistry]


  override def receive: Receive = {

    case b @ Bound(localAddress) ⇒ println(s"bound at $localAddress")

    case CommandFailed(_: Bind) ⇒ context stop self

    case c @ Connected(remote, local) ⇒
      val handler: ActorRef = context actorOf Frontend.props(clientRegistry)
      val connection: ActorRef = sender()

      connection ! Register(handler)
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

