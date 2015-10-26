package com.madsen.gameproto.akka

import akka.actor._
import com.madsen.gameproto.Protocol.ClientMessage
import com.madsen.gameproto.akka.InputProcessor.RunUpdate
import com.madsen.gameproto.akka.StateManager.StateUpdate

/**
 * Created by erikmadsen on 24/10/2015.
 */
class InputProcessor(stateManager: ActorRef) extends Actor {

  var updateStack: List[StateUpdate] = List.empty


  override def receive: Actor.Receive = {

    case RunUpdate ⇒
      updateStack.reverseIterator foreach { update ⇒
        stateManager ! update
      }
      updateStack = List.empty

    case message: ClientMessage ⇒
      val update: StateUpdate = transform(message)
      updateStack = update :: updateStack
  }


  private def transform(request: ClientMessage): StateUpdate = ???
}

object InputProcessor {

  def props(stateManager: ActorRef): Props = Props(classOf[InputProcessor], stateManager)


  case object RunUpdate


}