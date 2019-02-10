package com.agynamix.garten.snippet

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.User
import com.agynamix.garten.model.Client
import net.liftweb.common.Full
import com.agynamix.garten.lib.Gravatar
import net.liftweb.http.S
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import scala.xml.NodeSeq
import com.agynamix.garten.model.Membership
import com.agynamix.garten.config.Site
import com.agynamix.garten.config.App

object ClientTopbar {
  def render = {
    User.currentUser match {
      case Full(user) =>
        val clientConnections = Client.findAllByUser(user)
        val activeMembership = clientConnections.find(user.activeMembership.valueBox === _.id.get )
        val otherClientConnections = clientConnections.filterNot(user.activeMembership.valueBox === _.id.get )
        <ul class="nav navbar-nav" id="user">
          <li class="dropdown" data-dropdown="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">
              <i class="icon-book"></i> {S.?("Client")}:
              <span>{activeMembership.map(_.clientName).getOrElse("")}</span>
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu">
              { activeMembership.map(c =>
                  <li><a href={Site.home.url}><i class="icon-check"></i> <strong>{c.clientName}</strong></a></li>
                ).getOrElse(NodeSeq.Empty)}
              { if (otherClientConnections.nonEmpty) <li class="divider"></li> else NodeSeq.Empty }
              { for (clientConnection <- otherClientConnections) yield {
                  <li><a href="Javascript://" onclick={switchActiveClientConnection(user, clientConnection)}>
                    <i class="icon-check-empty"></i> {clientConnection.clientName}</a>
                  </li>
              }}
              {if (clientConnections.nonEmpty) {
                <li class="divider"></li>
                <li><a href={Site.clients.url} ><i class="icon-wrench"></i> Bearbeiten</a></li>
              } else {
                <li><a href={Site.clients.url}><i class="icon-asterisk"></i> Neuer Verein</a></li>
              }}
            </ul>
          </li>
        </ul>
      case _ => NodeSeq.Empty
    }
  }

  def switchActiveClient(user: User, client: Client) = {
    ajaxInvoke(()=>{
      println("Switching to client: "+client)
      user.switchActiveClient(client)
      S.redirectTo(App.requestInfo.map(_.uri).openOr(Site.dashboard.url))
    })._2.toJsCmd
  }

  def switchActiveClientConnection(user: User, clientConnection: Membership) = {
    ajaxInvoke(()=>{
      println("Switching to client: "+clientConnection)
      user.switchActiveMembership(clientConnection)
      S.redirectTo(App.requestInfo.map(_.uri).openOr(Site.dashboard.url))
    })._2.toJsCmd
  }

}
