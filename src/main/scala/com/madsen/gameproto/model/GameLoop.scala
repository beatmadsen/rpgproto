package com.madsen.gameproto.model

import scala.concurrent.Future

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
object GameLoop {

  val UpdateFps: Int = 60
  val MillisPerUpdate: Double = 1000.0 / UpdateFps


  def run(): Unit = {

    while (OnOffSwitch.isOn) {
      Ticker.tick()

      val lag: Double = Ticker.latestLag

      // line up user requested changes to game state
      processInput() // non-blocking, side-effecting -> Future[Unit]

      while (lag >= MillisPerUpdate) {
        // execute frame; movements, physics, ai
        update() // non-blocking, side-effecting -> Future[Unit]
        Ticker.commitWork(MillisPerUpdate)
      }

      render() // non-blocking, side-effecting -> Future[Unit]
    }
  }


  def render(): Future[Unit] = ???


  def update(): Future[Unit] = ???


  def processInput(): Future[Unit] = ???
}
