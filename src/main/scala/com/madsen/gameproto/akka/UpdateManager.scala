package com.madsen.gameproto.akka

import akka.actor._
import com.madsen.gameproto.akka.UpdateManager.{UpdateComplete, _}
import com.madsen.gameproto.model.LagTracker

import scala.concurrent.{ExecutionContextExecutor, Future}

/**
 * Singleton that manages updates vs lag
 */
class UpdateManager(stateManager: ActorRef) extends Actor {


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

object UpdateManager {

  def props(stateManager: ActorRef): Props = Props(classOf[UpdateManager], stateManager)


  case class UpdateInit(completeTo: ActorRef)


  case object UpdateComplete


}