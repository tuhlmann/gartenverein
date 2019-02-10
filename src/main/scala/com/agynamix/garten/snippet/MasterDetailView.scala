package com.agynamix.garten.snippet

import net.liftweb.http.S
import net.liftweb.util.Helpers._
import net.liftweb.common._
import com.agynamix.garten.lib.util.SnippetHelpers
import scala.xml.NodeSeq

object MasterDetailView extends SnippetHelpers {

  def render(in: NodeSeq): NodeSeq = {
    for {tplName <- S.attr("tpl")
         tpl <- S.runTemplate(List("templates-hidden", "parts", "form", tplName))
         ctrl <- S.attr("ctrl") } yield {

      val css =
        (for {header <- S.attr("h")} yield {
          "@heading-txt" #> header &
          "@heading [lift]" #> s"${ctrl}.createNew" &
          (for (btn <- S.attr("btn")) yield {
            "@new-screen *" #> btn
          }).openOr("@new-screen" #> "")
        }).openOr {
        "@heading" #> ("#createNewHeader ^*" #> "").apply(in)
      } &
        "@masterView [lift]" #> s"${ctrl}.masterView?eager_eval=true" &
        "@detailView [lift]" #> s"${ctrl}.detailView" &
        "@actions" #> ("#actions ^*" #> "").apply(in)

      css(tpl)

    }


  }

}