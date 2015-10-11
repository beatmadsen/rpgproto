package com.madsen.gameproto.model

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
object ExampleGameLoop {

  val UpdateFps: Int = 60
  val MillisPerUpdate: Double = 1000.0 / UpdateFps


  def run(): Unit = {

    while (OnOffSwitch.isOn) {

      // line up user requested changes to game state

      val steps: List[Future[Unit]] = processInput() ::
        updateTurn() ::
        render() ::
        Nil

      val f: Future[List[Unit]] = Future.sequence(steps)

      Await.result(f, Duration(MillisPerUpdate * 10, TimeUnit.MILLISECONDS))
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
