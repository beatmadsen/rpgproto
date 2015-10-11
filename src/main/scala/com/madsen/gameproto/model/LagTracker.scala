package com.madsen.gameproto.model

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
class LagTracker {

  private var previous: Double = currentTimeMs()
  private var lag: Double = 0d


  def commitWork(millis: Double): Unit = {
    lag -= millis
  }


  def tick(): Unit = {
    val current: Long = currentTimeMs()
    val elapsed: Double = current - previous

    previous = current
    lag += elapsed
  }


  private def currentTimeMs(): Long = System.currentTimeMillis()


  def latestLag: Double = {

    lag
  }
}
