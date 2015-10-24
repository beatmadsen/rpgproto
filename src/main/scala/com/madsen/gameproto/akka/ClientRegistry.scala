package com.madsen.gameproto.akka

import akka.actor.{Actor, ActorRef}

/**
 * Created by erikmadsen on 24/10/2015.
 */
object ClientRegistry {

  case class RegisterClient(id: String, proxy: ActorRef)

  case class UnregisterClient(id: String)

  case class ClientQuery(id: String)

  case class ClientResult(id: String, proxy: ActorRef)

}

class ClientRegistry extends Actor {

  import ClientRegistry._

  var clients: Map[String, ActorRef] = Map.empty


  override def receive: Actor.Receive = {

    case RegisterClient(id, proxy) ⇒
      clients += id → proxy

    case UnregisterClient(id) ⇒
      clients -= id

    case ClientQuery(id) ⇒
      val result: Option[ClientResult] = clients.get(id) map (ClientResult(id, _))
      sender() ! result
  }
}