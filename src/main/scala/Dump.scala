package com.madsen.gameproto.model

type SkillLevel = Long

















trait Updateable {

  def update(): Future[Unit]
}











