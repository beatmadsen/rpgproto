package com.madsen.gameproto.model

import com.madsen.gameproto.model.Battle.{BattleResult, BattleState}


trait Publisher {

  def publish(ongoing: BattleState): Unit

  def publish(complete: BattleResult): Unit
}


/**
The purpose is to update combat states one frame

  */
class BattleService(private val publisher: Publisher) {


  /**
   * Take given battle and advance it one frame
   */
  def advanceOneFrame(battle: BattleState): Unit = {

    // calculate frame - do ai moves
    if (battle.involved.isEmpty) {
      publisher.publish(BattleResult())
    } else {
      publisher.publish(battle.copy())
    }
  }
}
