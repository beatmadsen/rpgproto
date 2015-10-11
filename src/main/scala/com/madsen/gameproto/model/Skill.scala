package com.madsen.gameproto.model

import com.madsen.gameproto.model.Types.Guid

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
case class Skill (
  id: Guid,
  name: String,
  flavourTags: Set[Flavour]
)


trait Flavour {}