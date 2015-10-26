package com.madsen.gameproto.akka

import akka.actor._

/**
 * Created by erikmadsen on 24/10/2015.
 */
class StateManager extends Actor {

  override def receive: Actor.Receive = ???
}

object StateManager {


  trait StateUpdate

  case class UpdatePlayerLocation(playerId: String, x: Long, y: Long, z: Long) extends StateUpdate

}
