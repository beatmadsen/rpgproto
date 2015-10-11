package com.madsen.gameproto.model

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
object Battle {


  case class BattleState(
    involved: Set[Character],
    dead: Set[Character],
    withdrawn: Set[Character],
    bonuses: Map[Character, Bonus]
  )


  case class BattleResult()


  case class Bonus(
    rangedDamage: Int,
    meleeDamage: Int,
    remainingMillis: Long
  )


}
