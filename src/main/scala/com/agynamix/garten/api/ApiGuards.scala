package com.agynamix.garten.api

import net.liftweb.http.Req
import com.agynamix.garten.model.User

trait ApiGuards {

  def withLoggedIn: PartialFunction[Req, Unit] = {
    case _ if User.isLoggedIn =>
  }

}