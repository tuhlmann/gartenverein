package com.agynamix.garten.snippet

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.{DocumentFolder, Note}

/**
 * FIXME: Note is still visible to anyone logged in who knows the Note id!!
 * Add a guard that checks the note's recipient list and the client of the user to correspond with
 * that note's client and recipient list
 */
class OneDocumentFolder(folder: DocumentFolder) {

  def view = {
    "@subject *"    #> folder.title.asHtml &
    "@summary *"    #> folder.note.asHtml &
    "@author *"     #> folder.author.asHtml &
    "@created-at *" #> folder.createdAt.asHtml &
    "@due-date-row" #> "" &
    (if (folder.attachments.get.size > 0) {
      "@attachments *" #> folder.attachments.attachedDocuments(Empty, false)
    } else {
      "@attachments-block" #> ""
    })

  }


}
