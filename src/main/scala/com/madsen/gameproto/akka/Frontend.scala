package com.madsen.gameproto.akka

import akka.actor._
import akka.io.Tcp._
import akka.util.{ByteString, Timeout}
import com.madsen.gameproto.Protocol.{ClientMessage, Error, ServerMessage}
import com.madsen.gameproto.akka.ClientRegistry._
import play.api.libs.json._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Try}

/**
 * Created by erikmadsen on 24/10/2015.
 */
class Frontend(clientRegistry: ActorRef, inputProcessor: ActorRef) extends Actor with ActorLogging {

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
    val result: Try[ClientMessage] = tryParsing(data) recoverWith {
      case e: Exception ⇒
        log.error(e, "Could not parse input: {}", e.getMessage)
        Failure(e)
    }
    result.toOption
  }


  private def tryParsing(data: ByteString): Try[ClientMessage] = for {
    json ← Try(Json.parse(data.toArray))
    message ← Try(json.as[ClientMessage])
  } yield message


  private def handle(request: ClientMessage): Unit = {

    val client: ActorRef = sender()

    {
      request.login map { command ⇒
        clientRegistry ! RegisterClient(command.id, client)
        loggedIn = true
        ()
      }
    } orElse {
      request.logout map { command ⇒
        clientRegistry ! UnregisterClient(command.id)
        loggedIn = false
        ()
      }
    } getOrElse {
      (request, loggedIn) match {

        case (_, false) ⇒
          val byteString: ByteString = encodeError("Not logged in")
          client ! Write(byteString)

        case (message: ClientMessage, true) ⇒
          inputProcessor ! message

        case _ ⇒
          val byteString: ByteString = encodeError("Unexpected input received")
          client ! Write(byteString)
      }
      ()
    }
  }


  private def encodeError(errorMessage: String): ByteString = {
    val message = ServerMessage(errors = Some(Seq(Error(errorMessage))))
    val jsValue: JsValue = Json.toJson(message)
    val byteString = ByteString(jsValue.toString())
    byteString
  }
}


object Frontend {

  def props(clientRegistry: ActorRef, inputProcessor: ActorRef): Props =
    Props(classOf[Frontend], clientRegistry, inputProcessor)
}