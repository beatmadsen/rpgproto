package com.madsen.gameproto.model

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
case class Character (
  id: GUID,
  name: String,
  skills: Map[Skill, SkillLevel],
  feats: Set[Feat]
)
