package com.madsen.gameproto

import com.madsen.gameproto.Protocol._
import play.api.libs.json.{Format, JsValue, Json}

/**
 * Created by erikmadsen on 25/10/2015.
 */
object Protocol {

  case class ClientMessage(
    command: String,
    other: Option[Map[String, String]]
  )

  case class ServerMessage(
    errors: Option[Seq[Error]] = None,
    characters: Option[Map[String, CharacterState]] = None
  )

  case class Error(message: String)

  case class PrivateCharacterState(level: Int)

  case class Location(x: Long, y: Long, z: Long)

  case class CharacterState(location: Location, mPrivateCharacterState: Option[PrivateCharacterState])

  object ClientMessage {

    implicit val clientMessageFormat: Format[ClientMessage] = Json.format[ClientMessage]
  }

  object ServerMessage {

    implicit val errorFormat: Format[Error] = Json.format[Error]
    implicit val privateCharacterStateFormat: Format[PrivateCharacterState] = Json.format[PrivateCharacterState]
    implicit val locationFormat: Format[Location] = Json.format[Location]

    implicit val cStateFormat: Format[CharacterState] = Json.format[CharacterState]
    implicit val serverMessageFormat: Format[ServerMessage] = Json.format[ServerMessage]
  }

}


object ParsingTest extends App {

  import ServerMessage._

  val message = ServerMessage(Some(Seq(Error("a"), Error("b"))), None)

  val jsValue: JsValue = Json.toJson(message)

  println(jsValue.toString())
}