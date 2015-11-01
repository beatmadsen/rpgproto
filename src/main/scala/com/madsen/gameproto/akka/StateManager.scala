package com.madsen.gameproto.akka

import akka.actor._
import com.madsen.gameproto.akka.StateManager.UpdateComplete

/**
 * Created by erikmadsen on 24/10/2015.
 */
class StateManager extends Actor {

  override def receive: Actor.Receive = {
    case _ ⇒ sender() ! UpdateComplete // TODO
  }
}

object StateManager {

  trait StateUpdate

  trait StateUpdateLike[T] {

    def convert(t: T): StateUpdate
  }

  case class UpdatePlayerLocation(playerId: String, x: Long, y: Long, z: Long) extends StateUpdate

  case object NoUpdate extends StateUpdate

  case object UpdateComplete

}
