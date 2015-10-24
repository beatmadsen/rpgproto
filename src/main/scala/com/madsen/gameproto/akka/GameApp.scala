package com.madsen.gameproto.akka

import akka.actor._

import scala.io.StdIn

/**
 * Created by erikmadsen on 11/10/2015.
 */
object GameApp extends App {

  val system = ActorSystem("rpg-proto-system")

  val clientRegistry: ActorRef = system.actorOf(Props[ClientRegistry])
  val stateManager: ActorRef = system.actorOf(Props[StateManager])
  val inputProcessor: ActorRef = system.actorOf(InputProcessor.props(stateManager))
  val updateManager: ActorRef = system.actorOf(UpdateManager.props(stateManager))
  val renderer: ActorRef = system.actorOf(Renderer.props(stateManager, clientRegistry))
  val tcpEndpoint: ActorRef = system.actorOf(TcpEndpoint.props(clientRegistry, inputProcessor))
  val gameLoop: ActorRef = system.actorOf(GameLoop.props(updateManager, renderer, inputProcessor))


  println("Node started. Kill with 'q' + enter")
  Stream.continually(StdIn.readLine()).takeWhile(_ != "q")

  system.shutdown()
}






