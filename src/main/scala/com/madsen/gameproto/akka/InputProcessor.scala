package com.madsen.gameproto.akka

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.madsen.gameproto.Protocol.{ClientMessage, Location, MoveCommand}
import com.madsen.gameproto.akka.InputProcessor.{ProcessingComplete, RunProcessing}
import com.madsen.gameproto.akka.StateManager.{NoUpdate, StateUpdate, StateUpdateLike, UpdatePlayerLocation}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by erikmadsen on 24/10/2015.
 */
class InputProcessor(stateManager: ActorRef) extends Actor {

  import Timeout._

  implicit val ec: ExecutionContext = context.system.dispatcher
  implicit val timeout: Timeout = 30000L

  var updateStack: List[StateUpdate] = List.empty


  override def receive: Actor.Receive = {

    case RunProcessing ⇒
      val gameLoop: ActorRef = sender()

      Future.sequence {
        sendUpdates()
      } map { _ ⇒
        gameLoop ! ProcessingComplete
      }

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

    import ProtocolStateConversion._

    request.move map { command ⇒
      doConvert(command)
    } getOrElse NoUpdate
  }


  private def doConvert[T](t: T)(implicit ev: StateUpdateLike[T]): StateUpdate = ev.convert(t)
}

object InputProcessor {

  def props(stateManager: ActorRef): Props = Props(classOf[InputProcessor], stateManager)


  case object RunProcessing

  case object ProcessingComplete


}

object ProtocolStateConversion {

  implicit object StateUpdateLikeMoveCommand extends StateUpdateLike[MoveCommand] {
    override def convert(t: MoveCommand): StateUpdate = {
      val MoveCommand(characterId, Location(x, y, z)) = t

      UpdatePlayerLocation(characterId, x, y, z)
    }
  }

}