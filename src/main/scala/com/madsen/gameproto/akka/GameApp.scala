package com.madsen.gameproto.akka

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.madsen.gameproto.akka.UpdateManager._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.io.StdIn

/**
 * Created by erikmadsen on 11/10/2015.
 */
object GameApp extends App {

  val system = ActorSystem("rpg-proto-system")

  val clientRegistry: ActorRef = system.actorOf(Props[ClientRegistry])
  val stateManager: ActorRef = system.actorOf(Props[StateManager])
  val inputProcessor: ActorRef = system.actorOf(InputProcessor.props(stateManager))
  val tcpEndpoint: ActorRef = system.actorOf(TcpEndpoint.props(clientRegistry, inputProcessor))


  println("Node started. Kill with 'q' + enter")
  Stream.continually(StdIn.readLine()).takeWhile(_ != "q")

  system.shutdown()
}


object GameLoop {

  def props(updateManager: ActorRef): Props = Props(classOf[GameLoop], updateManager)
}

class GameLoop(updateManager: ActorRef) extends Actor {

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  import scala.concurrent.duration._

  implicit val timeout: Timeout = Timeout(1.minute)


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


  private def render(): Future[Unit] = {

    // TODO: Broadcast game state to all clients
    // (1) query all clients
    // (2) write json to each

    Future.successful(())
  }


  private def processInput(): Future[Unit] = ???


  case object NextTurn

}

