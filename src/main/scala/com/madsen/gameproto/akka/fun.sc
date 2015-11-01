import com.madsen.gameproto.Protocol._
import play.api.libs.json.{JsValue, Json}

val message = ServerMessage(
  List(Error("Not logged in")),
  Map("lebo" → CharacterState(Location(24, 24, 24), None)))
val jsValue: JsValue = Json.toJson(message)
//val byteString = ByteString(jsValue.toString())