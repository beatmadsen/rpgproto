package com.madsen.gameproto.model

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
object OnOffSwitch {
  private var on = true

  def isOn: Boolean = on

  def switchOff(): Unit = {
    on = false
  }
}
