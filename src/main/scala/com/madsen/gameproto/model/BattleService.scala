package com.madsen.gameproto.model

/**
The purpose is to update combat states one frame

  */
class BattleService(private val publisher: Publisher) {


  trait Publisher {

    def publish(ongoing: BattleState): Unit

    def publish(complete: BattleResult): Unit
  }


  /**
   * Take given battle and advance it one frame
   */
  def advanceOneFrame(battle: BattleState): Unit = {

    // calculate frame - do ai moves
    if (battle.involved.isEmpty) {
      publish(BattleResult())
    } else {
      publish(battle.copy())
    }
  }
}
