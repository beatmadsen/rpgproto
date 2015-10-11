package com.madsen.gameproto.model

import scala.concurrent.Future

object Types {

  type SkillLevel = Long

  type Guid = java.util.UUID
}


trait Updateable {

  def update(): Future[Unit]
}











