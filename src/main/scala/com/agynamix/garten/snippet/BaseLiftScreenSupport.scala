package com.agynamix.garten.snippet

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.http.RequestVar
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._

object ActiveModalScreenTab extends RequestVar[Box[String]](Empty)

trait ModalLiftScreenSupport[T <: MongoIdRecord[T]] extends StructuredLiftScreen[T] {

  self: StructuredLiftScreen[T] =>

  override protected def renderFormCmd: JsCmd = {
    super.renderFormCmd & ActiveModalScreenTab.is.map{ activeTab =>
      ShowTab("#"+activeTab)
    }.openOr(Noop) & showModalTooltips
  }

  override def screenTabActivated(tabId: String): JsCmd = {
    ActiveModalScreenTab.set(Full(tabId))
    Noop
  }


}