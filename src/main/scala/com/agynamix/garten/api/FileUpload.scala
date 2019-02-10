package com.agynamix.garten.api

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.PostRequest
import net.liftweb.http.NotFoundResponse
import net.liftweb.http.LiftRules
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import net.liftweb.http.RewriteResponse
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.http.JsonResponse
import net.liftweb.http.InMemoryResponse
import net.liftweb.http.S
import net.liftweb.http.OkResponse
import net.liftweb.http.Req
import com.agynamix.garten.model.User
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.util.Helpers
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.jquery.JqJsCmds.FadeOut
import net.liftweb.http.js.JsCmds.Replace
import scala.xml.NodeSeq
import net.liftweb.http.js.jquery.JqJsCmds.Hide
import net.liftweb.http.js.JsCmds.Run
import com.agynamix.garten.model.Note
import net.liftweb.http.StreamingResponse
import java.io.FileInputStream
import net.liftweb.http.TransientRequestVar
import net.liftweb.http.js.JsCmds
import com.agynamix.garten.model.Document
import com.agynamix.garten.config.{GardenConfig, App}
import net.liftweb.http.LiftResponse
import net.liftweb.http.FileParamHolder
import com.agynamix.garten.model.Client
import com.agynamix.garten.model.Membership

object FileUploadInProgress extends TransientRequestVar(false) {
  override lazy val __nameSalt = nextFuncName
}


object AsFileDownloadApiKey {
  def unapply(key: String): Option[String] = asFileDownloadApiKey(key)

  def asFileDownloadApiKey(key: String): Option[String] = {
    if (GardenConfig.fileDownloadApiKey == key) Some(key) else None
  }
}

object AsDocument {
  def unapply(guid: String): Option[Document] = asDocument(guid)

  def asDocument(guid: String): Option[Document] = Document.findDocument(guid)
}

object AsProtectedDocument {
  def unapply(guid: String): Option[Document] = asProtectedDocument(guid)

  def asProtectedDocument(guid: String): Option[Document] = {
    Document.findDocument(guid).flatMap(document => {
      User.currentUser.flatMap(user => {
        // Check user has access to this client
        Membership.findConnection(user.id.get, document.clientId.get).flatMap(membership => {
          // Can we somehow check if user has access to this particular document?
          Some(document)
        })
      })
    })
  }
}

object FileUpload extends RestHelper with Loggable {

  val downloadPath = List(App.hostAndPath, "api", "download")

  serve {
    case "api" :: "upload" :: Nil Post req => {
      val uploads = req.uploadedFiles
      logger.info(s"Uploaded files: ${uploads.map(_.fileName)}")
      val ojv: List[JObject] =
        uploads.map(fph => {
          val elemId = nextFuncName
          ("elemId" -> elemId) ~
          ("name" -> fph.fileName) ~
          ("size" -> fph.length) ~
          ("url" -> "#") ~
          ("thumbnail_url" -> "#") ~
          ("delete_url" -> "#") ~
          ("delete_type" -> "DELETE")
        })

      // run callbacks
      //S.session.map(_.runParams(req))

      JsonResponse("files" -> ojv).toResponse.asInstanceOf[InMemoryResponse]
    }

   // Redactor JS Image Upload
   case "api" :: "imgup" :: Nil Post AllowedImageMimeTypes(req) => uploadRedactorFile(req)

   // Redactor JS File Upload
   case "api" :: "fileup" :: Nil Post AllowedMimeTypes(req) => uploadRedactorFile(req)

    case "api" :: "delete" :: Nil Delete req => {
      logger.info("TODO: got a delete request, handle it!")
      OkResponse()
    }

  }

  def withUser(f: User=>LiftResponse): LiftResponse =
    User.currentUser.map(f).openOr(NotFoundResponse("No User Logged In"))

  def uploadRedactorFile(req: Req) = withUser (user => {
      val uploads = req.uploadedFiles
      logger.info(s"Redactor Uploaded files: ${uploads.map(_.fileName)}")
      val ojv: List[JObject] =
        uploads.flatMap(fph => {
          for {user <- User.currentUser
               clientId <- user.activeMembership.clientId
               docInfo <- Document.documentService.save(List(clientId.toString, user.id.get.toString), fph)} yield {
            val doc = Document.createInstance(user.id.get, user.id.get, clientId, docInfo).hidden(true).save(true)
            ("name" -> fph.fileName) ~
            ("guid" -> doc.guid.get.toString()) ~
            ("filelink" -> (downloadPath ::: List(doc.guid.get.toString())).mkString("/"))
          }
        })

      JsonResponse(ojv).toResponse.asInstanceOf[InMemoryResponse]
    })

  def init() = {
    //rewrite so the rest-callback will be a param instead to be fired with LiftSession.runParams
    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath("api" :: "upload" :: callback :: Nil, "", true, _), _, _) =>
           RewriteResponse("api" :: "upload" :: Nil, Map("callback" -> "_"))
    }
  }
}

object AllowedMimeTypes extends Loggable {
  def unapply(req: Req): Option[Req] = {
    logger.info("req.uploadedFiles.map{_.mimeType) is %s".format(req.uploadedFiles.map{_.mimeType}))
    req.uploadedFiles.flatMap{_.mimeType match {
      case "image/bmp"            => Some(req)
      case "image/x-windows-bmp"  => Some(req)
      case "image/vnd.dwg"        => Some(req)
      case "image/gif"            => Some(req)
      case "image/x-icon"         => Some(req)
      case "image/jpeg"           => Some(req)
      case "image/pict"           => Some(req)
      case "image/png"            => Some(req)
      case "image/x-quicktime"    => Some(req)
      case "image/tiff"           => Some(req)
      case "image/x-tiff"         => Some(req)
      case "application/pdf"      => Some(req)
      case "application/vnd.openxmlformats-officedocument.wordprocessingml.document" => Some(req)
      case "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" => Some(req)
      case "application/vnd.openxmlformats-officedocument.presentationml.presentation" => Some(req)
      case "application/vnd.ms-excel" => Some(req)
      case "application/vnd.ms-powerpoint" => Some(req)
      case xs                     => println("Unrecognized Mime Type 0: "+xs); None
    }}.headOption
  }
}

object AllowedImageMimeTypes extends Loggable {
  def unapply(req: Req): Option[Req] = {
    logger.info("req.uploadedFiles.map{_.mimeType) is %s".format(req.uploadedFiles.map{_.mimeType}))
    req.uploadedFiles.flatMap{_.mimeType match {
      case "image/gif"            => Some(req)
      case "image/jpeg"           => Some(req)
      case "image/png"            => Some(req)
      case xs                     => println("Unrecognized Mime Type 1: "+xs); None
    }}.headOption
  }
}

object AllowedTypes extends Loggable {

  def openDocumentAllowed(fph: FileParamHolder): Option[String] = {
    fph.mimeType match {
      case a @ "application/vnd.oasis.opendocument.text" => Some(a)
      case xs                     => println("Unrecognized Mime Type 3: "+xs); None
    }
  }

  def imagesAllowed(fph: FileParamHolder): Option[String] = {
    fph.mimeType match {
      case a @ "image/jpeg" => Some(a)
      case a @ "image/gif"  => Some(a)
      case a @ "image/png"  => Some(a)
      case xs               => println("Unrecognized Mime Type 3: "+xs); None
    }
  }

}