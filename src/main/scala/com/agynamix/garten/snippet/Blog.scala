package com.agynamix.garten.snippet

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.BlogEntry
import java.text.SimpleDateFormat
import com.agynamix.garten.lib.util.DateHelpers
import net.liftweb.util.CssSel

object Blog {

  def mainListing = {
    "@blog-entry" #> BlogEntry.findLatest(10).map(entry => {
      "@entry-created" #> ("Hinzugefügt: " + DateHelpers.germanShortDate.format(entry.createdAt.get)) &
      "@entry-link [href]" #> BlogEntry.url(entry) &
      "@entry-subject *" #> entry.subject.asHtml &
      "@entry-text *" #> entry.entry.shortHtml
    })
  }

  def toc: CssSel = toc(Empty)

  def toc(curEntry: Box[BlogEntry]) = {
    val curId = curEntry.map(_.id.get)
    "@toc-entry" #> BlogEntry.findLatest(100).map(entry => {
      "@created" #> DateHelpers.germanShortDate.format(entry.createdAt.get) &
        (if (curId === entry.id.get) {
          "@entry-link" #> entry.subject.asHtml
        } else {
          "@subject" #> entry.subject.asHtml &
          "@entry-link [href]" #> BlogEntry.url(entry)
        })
    })
  }

}

class OneBlogEntry(entry: BlogEntry) {

  def render = {
    "@entry-created" #> ("Hinzugefügt: " + DateHelpers.germanShortDate.format(entry.createdAt.get)) &
    "@entry-subject *" #> entry.subject.asHtml &
    "@entry-text *" #> entry.entry.asHtml andThen
    Blog.toc(Full(entry))
  }

}