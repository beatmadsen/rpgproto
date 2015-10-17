package com.madsen.gameproto.model

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
object ExampleGameLoop {

  val UpdateFps: Int = 60
  val MillisPerUpdate: Double = 1000.0 / UpdateFps
  val ticker = new LagTracker

  implicit val ec: ExecutionContextExecutor = ???


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

    ticker.tick()

    var lag: Double = ticker.latestLag
    while (lag >= MillisPerUpdate) {

      // execute frame; movements, physics, ai
      update() // BLOCKING!
      ticker.commitWork(MillisPerUpdate)

      lag = ticker.latestLag
    }
  }


  def update(): Unit = ???


  def render(): Future[Unit] = ???


  def processInput(): Future[Unit] = ???
}
