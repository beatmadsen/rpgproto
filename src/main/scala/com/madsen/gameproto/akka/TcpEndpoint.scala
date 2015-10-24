package com.madsen.gameproto.akka

import java.net.InetSocketAddress

import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}

/**
 * Created by erikmadsen on 24/10/2015.
 */
object TcpEndpoint {

  def props(clientRegistry: ActorRef,
    inputProcessor: ActorRef): Props = Props(classOf[TcpEndpoint], clientRegistry, inputProcessor)
}

class TcpEndpoint(clientRegistry: ActorRef, inputProcessor: ActorRef) extends Actor {

  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 9000))


  override def receive: Receive = {

    case b @ Bound(localAddress) ⇒ println(s"bound at $localAddress")

    case CommandFailed(_: Bind) ⇒ context stop self

    case c @ Connected(remote, local) ⇒
      val handler: ActorRef = context actorOf Frontend.props(clientRegistry, inputProcessor)
      val connection: ActorRef = sender()

      connection ! Register(handler)
  }
}