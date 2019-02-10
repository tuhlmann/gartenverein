package com.agynamix.garten.snippet

import com.agynamix.garten.model.Garden
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import net.liftweb.http.S
import net.liftweb.util.Helpers
import scala.xml.Group
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.SiteMap
import scala.xml.Elem
import net.liftweb.sitemap.ConvertableLoc
import scala.xml.Text
import net.liftweb.http.RequestVar
import net.liftweb.http.LiftRules
import net.liftweb.sitemap.Menu
import com.agynamix.garten.config.Site
import net.liftweb.sitemap.Menu.ParamMenuable
import com.agynamix.garten.model.Note
import com.agynamix.garten.model.Document
import com.agynamix.garten.model.GeneratedDocument

/**
 * FIXME: Note is still visible to anyone logged in who knows the Note id!!
 * Add a guard that checks the note's recipient list and the client of the user to correspond with
 * that note's client and recipient list
 */
class OneDocument(doc: Document) {

  def view = {
    "@name *"    #> doc.displayName.asHtml &
    "@download [href]" #> doc.downloadDocumentUrl() &
    "@type *"    #> doc.mimeType.asHtml &
    "@author *"     #> doc.author.asHtml &
    "@recipient *"     #> doc.userId.asHtml &
    "@created-at *" #> doc.createdAt.asHtml &
    (for (genDoc <- GeneratedDocument.findByDocumentId(doc.id.get)) yield {
      "@subject" #> genDoc.getSubject() &
      (if (genDoc.note.get.nonEmpty) {
        "@description" #>  genDoc.note.get
      } else {
        "@description-block" #> ""
      })
    }).openOr {
      "@details" #> ""
    }
  }


}
