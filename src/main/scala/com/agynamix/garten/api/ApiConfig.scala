package com.agynamix.garten.api

import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers._

object ApiConfig extends ApiGuards {

  def init(): Unit = {
	  LiftRules.dispatch.append(withLoggedIn guard FileUpload)
    LiftRules.dispatch.append(FileDownload)

    FileUpload.init()
  }

}