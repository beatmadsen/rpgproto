package com.madsen.gameproto.model

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
object Ticker {

  private var previous: Double = 0d
  private var lag: Double = 0d

  def commitWork(millis: Double): Unit = {
    lag -= millis
  }


  def currentTimeMs(): Long = ???


  def tick(): Unit = {
    val current: Long = currentTimeMs()
    val elapsed: Double = current - previous

    previous = current
    lag += elapsed
  }

  def latestLag: Double = {

    lag
  }
}
