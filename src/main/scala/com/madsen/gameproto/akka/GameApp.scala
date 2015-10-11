package com.madsen.gameproto.akka

import akka.actor.{Actor, ActorSystem, Props}
import com.madsen.gameproto.model.Ticker

import scala.concurrent.Future
import scala.io.StdIn

/**
 * Created by erikmadsen on 11/10/2015.
 */
object GameApp extends App {

  val system = ActorSystem("rpg-proto-system")

  system.actorOf(Props[GameLoop], "game-service")

  println("Node started. Kill with 'q' + enter")
  Stream.continually(StdIn.readLine()).takeWhile(_ != "q")

  readLine("Press ENTER to exit")

  system.shutdown()
}


case object NextTurn

case object Start

class GameLoop extends Actor {

  val UpdateFps: Int = 60
  val MillisPerUpdate: Double = 1000.0 / UpdateFps


  self ! NextTurn


  override def receive: Receive = {
    case NextTurn ⇒ runOneTurn()
  }


  private def runOneTurn(): Unit = {


    // line up user requested changes to game state

    val steps: List[Future[Unit]] = processInput() ::
      updateTurn() ::
      render() ::
      Nil

    val f: Future[List[Unit]] = Future.sequence(steps)

    f foreach { _ ⇒
      self ! NextTurn
    }
  }


  def updateTurn(): Future[Unit] = Future {
    // NB: Future.apply() is different in scala.concurrent/akka than in Finagle.
    Ticker.tick()

    var lag: Double = Ticker.latestLag
    while (lag >= MillisPerUpdate) {

      // execute frame; movements, physics, ai
      update() // BLOCKING!
      Ticker.commitWork(MillisPerUpdate)

      lag = Ticker.latestLag
    }
  }


  def update(): Unit = ???


  def render(): Future[Unit] = ???


  def processInput(): Future[Unit] = ???
}