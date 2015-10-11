package com.madsen.gameproto.model

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
case class Feat (
  id: Guid,
  name: String,
  requirements: Map[Skill, SkillLevel]
)
