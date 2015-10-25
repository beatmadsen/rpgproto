package com.madsen.gameproto.akka

import akka.actor._

/**
 * Created by erikmadsen on 24/10/2015.
 */
class InputProcessor(stateManager: ActorRef) extends Actor {


  override def receive: Actor.Receive = ???
}

object InputProcessor {

  def props(stateManager: ActorRef): Props = Props(classOf[InputProcessor], stateManager)

  trait StateUpdate

  case class UpdatePlayerLocation(x: Long, y: Long, z: Long) extends StateUpdate


}