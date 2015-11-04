package com.madsen.gameproto.akka

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.madsen.gameproto.Protocol.{ClientMessage, Location, MoveCommand}
import com.madsen.gameproto.akka.InputProcessor.{ClearStack, ProcessingComplete, RunProcessing}
import com.madsen.gameproto.akka.StateManager.{NoUpdate, StateUpdate, UpdatePlayerLocation}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by erikmadsen on 24/10/2015.
  */
class InputProcessor(stateManager: ActorRef) extends Actor {

  import ProtocolStateConversion.StateUpdateLikeMoveCommand
  import Timeout._

  implicit val ec: ExecutionContext = context.system.dispatcher
  implicit val timeout: Timeout = 30000L

  // Compile error on implicits if this is not called
  StateUpdateLikeMoveCommand

  var updateStack: List[StateUpdate] = List.empty


  override def receive: Actor.Receive = {

    case RunProcessing ⇒
      val gameLoop: ActorRef = sender()

      Future.sequence {
        sendUpdates()
      } foreach { _ ⇒
        self ! ClearStack
        gameLoop ! ProcessingComplete
      }

    case ClearStack ⇒
      updateStack = List.empty

    case message: ClientMessage ⇒
      val update: StateUpdate = transform(message)
      updateStack = update :: updateStack
  }


  private def sendUpdates(): Iterator[Future[Any]] = {
    updateStack.reverseIterator map { update ⇒
      stateManager ? update
    }
  }


  private def transform(request: ClientMessage): StateUpdate = {

    import InputProcessor.doConvert

    request.move map doConvert[MoveCommand] getOrElse NoUpdate
  }
}

object InputProcessor {

  def props(stateManager: ActorRef): Props = Props(classOf[InputProcessor], stateManager)

  import com.madsen.gameproto.akka.StateManager.StateUpdateLike

  def doConvert[T: StateUpdateLike](t: T): StateUpdate = {
    implicitly[StateUpdateLike[T]].convert(t)
  }


  case object RunProcessing

  case object ClearStack

  case object ProcessingComplete

}


object ProtocolStateConversion {

  import StateManager.StateUpdateLike

  implicit object StateUpdateLikeMoveCommand extends StateUpdateLike[MoveCommand] {
    override def convert(t: MoveCommand): StateUpdate = {
      val MoveCommand(characterId, Location(x, y, z)) = t

      UpdatePlayerLocation(characterId, x, y, z)
    }
  }

}


