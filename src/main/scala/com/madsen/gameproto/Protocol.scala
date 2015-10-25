package com.madsen.gameproto

import play.api.libs.json.{Format, Json}

/**
 * Created by erikmadsen on 25/10/2015.
 */
object Protocol {

  case class ClientMessage(
    command: String,
    arguments: Map[String, String]
  )

  case class ServerMessage(
    errors: Seq[Error],
    characters: Map[String, CharacterState]
  )

  case class Error(message: String)

  case class CharacterState(
    location: Location,
    privateState: Option[PrivateCharacterState]
  )

  case class PrivateCharacterState(level: Int)

  case class Location(x: Long, y: Long, z: Long)

  object ClientMessage {

    implicit val clientMessageFormat: Format[ClientMessage] = Json.format[ClientMessage]
  }

  object ServerMessage {

    implicit val serverMessageFormat: Format[ServerMessage] = Json.format[ServerMessage]
    implicit val errorFormat: Format[Error] = Json.format[Error]
    implicit val characterStateFormat: Format[CharacterState] = Json.format[CharacterState]
    implicit val pCharacterStateFormat: Format[PrivateCharacterState] = Json.format[PrivateCharacterState]
    implicit val locationFormat: Format[Location] = Json.format[Location]
  }

}
