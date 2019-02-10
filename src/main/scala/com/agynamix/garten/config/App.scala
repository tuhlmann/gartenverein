package com.agynamix.garten.config

import net.liftweb.http.RequestVar
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.sitemap.Loc.AnyLocParam
import net.liftweb.sitemap.Loc
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes._

/**
 * The PageMode. That value reflects the current page that is displayed for the current user.
 * This value should be used (@see Sg.pageMode) instead of constructing your own.
 * Please not that the index of the value in the enumeration is saved in the database, so if you need to
 * add more page locations, please add them to the end of the enum.
 */
//abstract class AppArea(val name: String, val id: Int) extends AnyLocParam

//object ApplicationArea {
//
//  case object Auto        extends AppArea("auto", 1)
//  case object Public      extends AppArea("public", 2)
//  case object Account     extends AppArea("account", 3)
//  case object Management  extends AppArea("management", 4)
//  case object Billing     extends AppArea("billing", 5)
//  case object Association extends AppArea("association", 6)
//  case object AllGardens  extends AppArea("gardens", 7)
//  case object OneGarden   extends AppArea("garden", 8)
//  case object Members     extends AppArea("members", 9)
//
//}

// hostAndPath: String,
case class RequestInfo(uri: String)

//case class Area(area: AppArea) extends AnyLocParam

object App extends Loggable {

  object requestInfo extends RequestVar[Box[RequestInfo]](Empty) {
    override def logUnreadVal = false
  }

//  object pageMode extends RequestVar[AppArea](initializePageMode(requestInfo.is)) {
//    override def logUnreadVal = false
//  }

  // This is to capture the host and path of this instance to be able to use it in actors, where S is not available
  // requestInfo.is.map(_.hostAndPath) openOr
  def hostAndPath: String = Props.get("local.hostname").openOr(Props.mode match {
    case Development => "http://localhost:8080"
    case _           => "http://www.unser-gartenverein.de"
  })

//  def initializePageMode(requestInfo: Box[RequestInfo]): AppArea = {
//    requestInfo match {
//      case Full(RequestInfo(s)) if s.contains("/dashboard")    => ApplicationArea.Management
//      case Full(RequestInfo(s)) if s.contains("/association")  => ApplicationArea.Association
//      case Full(RequestInfo(s)) if s.contains("/gardens")      => ApplicationArea.AllGardens
//      case Full(RequestInfo(s)) if s.contains("/garden/")      => ApplicationArea.OneGarden
//      case Full(RequestInfo(s)) if s.contains("/settings")     => ApplicationArea.Account
//      case Full(RequestInfo(s)) if s.contains("/management")   => ApplicationArea.Management
//      case _ => ApplicationArea.Auto
//    }
//  }
//
//  def area(loc: Loc[_]): Box[AppArea] = loc.params.filter(p => p.isInstanceOf[AppArea]).map(_.asInstanceOf[AppArea]).headOption


}