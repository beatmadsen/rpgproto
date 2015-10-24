package com.madsen.gameproto.akka

import akka.actor._

/**
 * Created by erikmadsen on 24/10/2015.
 */
class Renderer(stateManager: ActorRef, clientRegistry: ActorRef) extends Actor {

  override def receive: Actor.Receive = ???
}

object Renderer {

  def props(
    stateManager: ActorRef,
    clientRegistry: ActorRef
  ): Props = Props(classOf[Renderer], stateManager, clientRegistry)
}