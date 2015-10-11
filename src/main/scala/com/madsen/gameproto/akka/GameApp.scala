package com.madsen.gameproto.akka

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import com.madsen.gameproto.akka.UpdateProtocol.{UpdateComplete, _}
import com.madsen.gameproto.model.LagTracker

import scala.concurrent.Future
import scala.io.StdIn

/**
 * Created by erikmadsen on 11/10/2015.
 */
object GameApp extends App {

  val system = ActorSystem("rpg-proto-system")

  system.actorOf(Props[GameLoop], "game-loop")

  println("Node started. Kill with 'q' + enter")
  Stream.continually(StdIn.readLine()).takeWhile(_ != "q")

  system.shutdown()
}


class GameLoop extends Actor {


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