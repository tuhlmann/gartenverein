package com.agynamix.garten.snippet

import net.liftweb.util.Helpers._
import net.liftweb.common._
import org.joda.time.DateTime
import com.agynamix.garten.model.{GeneratedDocument, Garden, Membership, User}
import com.agynamix.garten.lib.util.{SnippetHelpers, LocalizedEnum}
import org.joda.time.format.DateTimeFormat
import java.util.Locale
import net.liftweb.http.SHtml._
import net.liftweb.http.S
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds.{Show, FadeIn}
import scala.xml.NodeSeq

object Dashboard extends SnippetHelpers {

  val MAX_IMAGES = 25
  val IMG_SRC_PATH = "/media/img/day/"

  object DayOfWeek extends Enumeration with LocalizedEnum {
    type DayOfWeek = Value

    val locKeyBase = "dashboard.day_of_week"

    val Monday     = Value(1, "monday")
    val Tuesday    = Value(2, "tuesday")
    val Wednesday  = Value(3, "wednesday")
    val Thursday   = Value(4, "thursday")
    val Friday     = Value(5, "friday")
    val Saturday   = Value(6, "saturday")
    val Sunday     = Value(7, "sunday")

    def displayValue(t: DayOfWeek.Value) = localized(t)

  }

  def imagesOfDay = {
    val date = new DateTime(User.getDateTimeZone)
    val fmt = DateTimeFormat.forPattern("dd.MMMM yyyy");
    val dayOfWeek = DayOfWeek(date.getDayOfWeek)
    val rnd1 = randomInt(MAX_IMAGES)+1
    val rnd2 = {
      val r = randomInt(MAX_IMAGES)+1
      if (rnd1 == r) ((rnd1+1) % MAX_IMAGES)+1 else r
    }
    val postfix = if (((rnd1+rnd2) % 2) == 0) "_p" else "_l"

    "@image-1 [src]" #> s"${IMG_SRC_PATH}day_${rnd1}${postfix}.jpg" &
    "@image-2 [src]" #> s"${IMG_SRC_PATH}day_${rnd2}${postfix}.jpg" &
    "@statement *" #> {
      <div>
        <p>Heute ist {DayOfWeek.displayValue(dayOfWeek)},</p>
        <p>der {fmt.withLocale(User.getLocale).print(date)}</p>
      </div>
    }
  }

  def search = {
    (for {
      user <- User.currentUser
      clientId <- user.activeMembership.clientId
    } yield {
      "#db-multi-search" #> onSubmit(searchCase => {
        (if (searchCase.trim.nonEmpty) {
          val search = searchCase.trim.toLowerCase

          val ms = new Memberships()
          ms.listPaginator.setSearchQuery(search)
          val members = ms.listPaginator.page

          val gardensSnip = new Gardens()
          gardensSnip.listPaginator.setSearchQuery(search)
          val gardens = gardensSnip.listPaginator.page // Garden.multiSearch(user, clientId, search)

          val docSnip = new GeneratedDocuments()
          docSnip.listPaginator.setSearchQuery(search)
          val documents = docSnip.listPaginator.page

          val html = processResults(searchCase, members, gardens, documents)
          SetHtml("db-multi-search-results", html) &
          new FadeIn("db-multi-search-results", 0 seconds, 1 second)
        } else Noop) &
        SetValById("db-multi-search", "") // clear the input box
      })
    }) openOr {
      "#db-multi-search-area" #> ""
    }
  }

  def processResults(search: String, members: List[Membership], gardens: List[Garden], documents: List[GeneratedDocument]): NodeSeq = {
    val tpl = partTpl("multi-search-result")

    val css = {
      "@db-multi-search-term" #> search &
      "@db-multi-search-result-count" #> (members.size + gardens.size + documents.size) &
      "@db-multi-search-result-header-members-a [href]" #> s"/members?filter=${search}" &
      "@db-multi-search-result-header-members-count" #> members.size &
      "@db-multi-search-result-members *" #> members.map(ms => {
        "@db-multi-search-result-members-a *" #> ms.displayName &
        "@db-multi-search-result-members-a [href]" #> s"/members?filter=${search}&select=${ms.id.get}"
      }) &
      "@db-multi-search-result-header-gardens-a [href]" #> s"/gardens?filter=${search}" &
      "@db-multi-search-result-header-gardens-count" #> gardens.size &
      "@db-multi-search-result-gardens *" #> gardens.map(g => {
        "@db-multi-search-result-gardens-a *" #> <span>{g.garden_no.asHtml} ({g.gardenOwners.asHtml})</span> &
        "@db-multi-search-result-gardens-a [href]" #> s"/gardens?filter=${search}&select=${g.id.get}"
      }) &
      "@db-multi-search-result-header-documents-a [href]" #> s"/management/documents?filter=${search}" &
      "@db-multi-search-result-header-documents-count" #> documents.size &
      "@db-multi-search-result-documents *" #> documents.map(d => {
        "@db-multi-search-result-documents-a *" #> <span>{d.subject.asHtml} ({d.author.asHtml})</span> &
        "@db-multi-search-result-documents-a [href]" #> s"/management/documents?filter=${search}&select=${d.id.get}"
      })
    }
    css(tpl)

  }

}
