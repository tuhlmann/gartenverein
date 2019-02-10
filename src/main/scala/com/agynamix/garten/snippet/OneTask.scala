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
import com.agynamix.garten.model.Task

/**
 * FIXME: Note is still visible to anyone logged in who knows the Note id!!
 * Add a guard that checks the note's recipient list and the client of the user to correspond with
 * that note's client and recipient list
 */
class OneTask(task: Task) {

  def view = {
    "@subject *"    #> task.subject.asHtml &
    "@summary *"    #> task.note.asHtml &
    "@author *"     #> task.author.asHtml &
    "@created-at *" #> task.createdAt.asHtml &
    (if (task.dueDate.valueBox.isDefined) {
      "@due-date *" #> task.dueDate.asHtml
    } else {
      "@due-date-row" #> ""
    }) &
    (if (task.attachments.get.size > 0) {
      "@attachments *" #> task.attachments.attachedDocuments(Empty, false)
    } else {
      "@attachments-block" #> ""
    })

  }


}
