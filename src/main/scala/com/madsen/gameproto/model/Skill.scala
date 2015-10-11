package com.madsen.gameproto.model

/**
 * Created by erikmadsen2 on 11/10/2015.
 */
case class Skill (
  id: Guid,
  name: String,
  flavourTags: Set[Flavour]
)
