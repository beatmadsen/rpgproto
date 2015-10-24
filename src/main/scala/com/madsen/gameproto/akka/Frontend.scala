package com.madsen.gameproto.akka

import akka.actor._
import akka.io.Tcp._
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.madsen.gameproto.akka.ClientRegistry._
import play.api.libs.json._

import scala.concurrent.ExecutionContext
import scala.util.Try

/**
 * Created by erikmadsen on 24/10/2015.
 */
class Frontend(clientRegistry: ActorRef) extends Actor {

  val Separator: Seq[Byte] = ByteString("\r\n").toArray.toSeq
  implicit val timeout: Timeout = Timeout.longToTimeout(8000)
  implicit val ec: ExecutionContext = context.system.dispatcher
  var count = 0
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


  private def parse(data: ByteString): Option[RpgRequest] = {
    Try {
      Json.parse(data.toArray) // TODO: Recover by logging and passing along error
    }.toOption flatMap (jsValue ⇒ jsValue.asOpt[RpgRequest])
  }


  private def handle(request: RpgRequest): Unit = {

    val oldS = sender()

    request match {

      case RpgRequest("hello", args) ⇒
        args.get("id") foreach { id ⇒
          clientRegistry ! RegisterClient(id, sender())
        }

      case RpgRequest("check", args) ⇒
        args.get("id") foreach { id ⇒
          (clientRegistry ? ClientQuery(id)).mapTo[Option[ClientResult]] foreach { mResult ⇒
            oldS ! Write(ByteString(mResult.toString))
          }
        }

      case RpgRequest("plus", args) ⇒
        args.get("value") foreach { value ⇒
          count += value.toInt
        }

      case RpgRequest("minus", args) ⇒
        args.get("value") foreach { value ⇒
          count -= value.toInt
        }

      case RpgRequest("get", _) ⇒

        val response: RpgResponse = RpgResponse(count)
        val jsonOut = Json.toJson(response)

        sender() ! Write(ByteString(jsonOut.toString()))

      case _ ⇒ sender ! Write(ByteString("Unexpected input"))
    }
  }
}

object Frontend {

  def props(clientRegistry: ActorRef): Props = Props(classOf[Frontend], clientRegistry)
}