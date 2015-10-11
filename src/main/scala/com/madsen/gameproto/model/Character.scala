package com.madsen.gameproto.model

import com.madsen.gameproto.model.Types.{Guid, SkillLevel}

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
case class Character(
  id: Guid,
  name: String,
  skills: Map[Skill, SkillLevel],
  feats: Set[Feat]
)
