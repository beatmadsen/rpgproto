package com.madsen.gameproto.akka

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.madsen.gameproto.akka.UpdateManager._

import scala.concurrent.{ExecutionContextExecutor, Future}

/**
 * Created by erikmadsen on 24/10/2015.
 */
class GameLoop(
  updateManager: ActorRef,
  renderer: ActorRef,
  inputProcessor: ActorRef
) extends Actor {

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  import scala.concurrent.duration._

  implicit val timeout: Timeout = Timeout(1.minute)

  self ! NextTurn


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

object GameLoop {

  def props(
    updateManager: ActorRef,
    renderer: ActorRef,
    inputProcessor: ActorRef
  ): Props = Props(classOf[GameLoop], updateManager, renderer, inputProcessor)
}