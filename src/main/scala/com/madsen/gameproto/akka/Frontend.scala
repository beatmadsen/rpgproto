package com.madsen.gameproto.akka

import akka.actor._
import akka.io.Tcp._
import akka.util.{ByteString, Timeout}
import com.madsen.gameproto.Protocol.{ClientMessage, Error, ServerMessage}
import com.madsen.gameproto.akka.ClientRegistry._
import com.madsen.gameproto.akka.InputProcessor.StateUpdate
import play.api.libs.json._

import scala.concurrent.ExecutionContext
import scala.util.Try

/**
 * Created by erikmadsen on 24/10/2015.
 */
class Frontend(clientRegistry: ActorRef, inputProcessor: ActorRef) extends Actor {

  val Separator: Seq[Byte] = ByteString("\r\n").toArray.toSeq
  implicit val timeout: Timeout = Timeout.longToTimeout(8000)
  implicit val ec: ExecutionContext = context.system.dispatcher

  var loggedIn = false


  var buffer: ByteString = ByteString.empty


  override def receive: Receive = {

    case Received(data) ⇒ onReceive(data)
    case PeerClosed ⇒ context stop self
  }


  private def onReceive(data: ByteString): Unit = {

    buffer ++= data

    val indexOfSlice: Int = buffer.indexOfSlice(Separator)

    if (indexOfSlice == -1) ()
    else {

      val (_, b) = buffer.splitAt(indexOfSlice + Separator.size)
      buffer = b

      parse(data) foreach handle

      onReceive(ByteString.empty) // check for additional commands in buffer
    }
  }


  private def parse(data: ByteString): Option[ClientMessage] = {
    Try {
      Json.parse(data.toArray) // TODO: Recover by logging and passing along error
    }.toOption flatMap (jsValue ⇒ jsValue.asOpt[ClientMessage])
  }


  // TODO: Frontend translates external messages into internal messages for the input processing
  private def handle(request: ClientMessage): Unit = {

    val client: ActorRef = sender()

    (request, loggedIn) match {

      case (ClientMessage("logon", args), _) ⇒
        args.get("id") foreach { id ⇒
          clientRegistry ! RegisterClient(id, client)
          loggedIn = true
        }

      case (ClientMessage("logout", args), _) ⇒
        args.get("id") foreach { id ⇒
          clientRegistry ! UnregisterClient(id)
          loggedIn = false
        }

      case (_, false) ⇒
        val message = ServerMessage(Seq(Error("Not logged in")), Map.empty)
        val byteString = ByteString(Json.toJson(message).toString())
        client ! Write(byteString)

      case (message: ClientMessage, true) ⇒
        val update: StateUpdate = transform(message)
        inputProcessor ! update

      case _ ⇒
        val message = ServerMessage(Seq(Error("Unexpected input")), Map.empty)
        val byteString = ByteString(Json.toJson(message).toString())
        client ! Write(byteString)
    }
  }


  private def transform(request: ClientMessage): StateUpdate = ???
}


object Frontend {

  def props(clientRegistry: ActorRef, inputProcessor: ActorRef): Props =
    Props(classOf[Frontend], clientRegistry, inputProcessor)
}