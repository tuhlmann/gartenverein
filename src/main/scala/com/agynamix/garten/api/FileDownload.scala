package com.agynamix.garten.api

import net.liftweb.http.rest.RestHelper
import net.liftweb.http.LiftRules
import net.liftweb.common._
import net.liftweb.http.StreamingResponse
import com.agynamix.garten.model.User
import net.liftweb.http.RedirectWithState
import com.agynamix.garten.config.GardenConfig
import net.liftweb.http.RedirectState
import com.agynamix.garten.config.LoginRedirect
import net.liftweb.http.NotFoundResponse
import net.liftweb.http.ForbiddenResponse
import net.liftweb.http.RedirectResponse
import com.agynamix.garten.config.Site

object FileDownload extends RestHelper with Loggable {

  serve {

    case "api" :: "download" :: AsDocument(document) :: AsFileDownloadApiKey(apiKey) :: Nil Get req => {
      val headers = ("Content-Type", document.mimeType.get) ::
                    ("Content-Disposition", ("inline; filename=\""+document.fileName.get+"\"")) ::
                    ("Content-length" -> document.length.get.toString) :: Nil //, ("Content-disposition", "attachment"))
      ()=>Full(StreamingResponse(document.documentUrl.openStream(), () => (), document.length.get, headers, Nil, 200))
    }

    case "api" :: "download" :: documentId :: Nil Get req => {
      if (!User.isLoggedIn) {
        val uri = req.uri
        RedirectWithState(GardenConfig.loginUrl.vend, RedirectState(() => { LoginRedirect.set(Full(uri)) }))
      } else {
        AsProtectedDocument.asProtectedDocument(documentId).map(document => {
          val headers = ("Content-Type", document.mimeType.get) ::
                        ("Content-Disposition", ("inline; filename=\""+document.fileName.get+"\"")) ::
                        ("Content-length" -> document.length.get.toString) :: Nil //, ("Content-disposition", "attachment"))
          ()=>Full(StreamingResponse(document.documentUrl.openStream(), () => (), document.length.get, headers, Nil, 200))
        }).getOrElse(()=>Full(RedirectResponse("/404")))
      }
    }

  }

}