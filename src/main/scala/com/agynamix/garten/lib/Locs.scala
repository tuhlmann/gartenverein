package com.agynamix.garten.lib

import net.liftweb._
import common._
import http.{RedirectResponse, RedirectWithState, S, RedirectState}
import sitemap.{Loc, Menu}
import sitemap.Loc.{DispatchLocSnippets, EarlyResponse, If}
import net.liftweb.sitemap.Loc.LocParam
import com.agynamix.garten.config.GardenConfig
import com.agynamix.garten.config.LoginRedirect
import com.agynamix.garten.model.User
import com.agynamix.garten.config.Site
import com.agynamix.garten.config.RolesDef
import com.agynamix.garten.config.Permissions
import net.liftweb.sitemap.Loc._

object Locs extends Locs
trait Locs {
  private lazy val userMeta           = GardenConfig.authUserMeta.vend

  private lazy val indexUrl           = GardenConfig.indexUrl.vend
  private lazy val loginUrl           = GardenConfig.loginUrl.vend
  private lazy val logoutUrl          = GardenConfig.logoutUrl.vend
  private lazy val loginTokenUrl      = GardenConfig.loginTokenUrl.vend

  private lazy val invitationTokenUrl = GardenConfig.invitationTokenUrl.vend

  // redirects
  def RedirectToLoginWithReferrer = {
    val uri = S.uriAndQueryString
    RedirectWithState(loginUrl, RedirectState(() => { LoginRedirect.set(uri) }))
  }

  def RedirectToIndex = RedirectResponse(indexUrl)
  def RedirectToDashboard = RedirectResponse("/dashboard")
  def RedirectToIndexWithCookies = RedirectResponse(indexUrl, S.responseCookies:_*)

  protected def DisplayError(message: String) = () =>
    RedirectWithState(indexUrl, RedirectState(() => S.error(message)))

  // Loc guards
  val RequireAuthentication = If(
    () => userMeta.isAuthenticated,
    () => RedirectToLoginWithReferrer)

  val RequireNoAuthentication = If(
    () => !userMeta.isAuthenticated,
    () => RedirectToIndex)

  val RequireLoggedIn = If(
    () => userMeta.isLoggedIn,
    () => RedirectToLoginWithReferrer)

  val RequireNotLoggedIn = If(
    () => !userMeta.isLoggedIn,
    () => RedirectToIndex)

  val RedirectToDashBoardWhenLoggedIn = If(
    () => !userMeta.isLoggedIn,
    () => RedirectToDashboard)

  val RequireHasClient = If(
    () => User.currentUser.map(_.activeMembership.valueBox.isDefined).openOr(false),
    () => S.redirectTo(Site.home.url) )

  val RequireSuperUser = If(
    () => userMeta.hasPermission(Permissions.SuperUser),
    () => S.redirectTo(Site.home.url) )

  case object NoopLoc extends LocParam[Any]

//  def HasRole(role: String) =
//    If(() => userMeta.hasRole(role),
//      DisplayError("You are the wrong role to access that resource."))

//  def LacksRole(role: String) =
//    If(() => userMeta.lacksRole(role),
//      DisplayError("You lack the sufficient role to access that resource."))

  def HasPermission(permission: Permission) =
    If(() => userMeta.hasPermission(permission),
      DisplayError("Insufficient permissions to access that resource."))

  def LacksPermission(permission: Permission) =
    If(() => userMeta.lacksPermission(permission),
      DisplayError("Overqualified permissions to access that resource."))

//  def HasAnyRoles(roles: Seq[String]) =
//    If(() => userMeta.hasAnyRoles(roles),
//       DisplayError("You are the wrong role to access that resource."))

  // Menus
  def buildLogoutMenu = Menu(Loc(
    "Logout",
    logoutUrl.split("/").filter(_.length > 0).toList,
    S.?("logout"), logoutLocParams
  ))

  protected def logoutLocParams = RequireLoggedIn ::
    EarlyResponse(() => {
      if (userMeta.isLoggedIn) {
        userMeta.logUserOut() }
      Full(RedirectToIndexWithCookies)
    }) :: Nil


  def buildLoginTokenMenu = Menu(Loc(
    "LoginToken", loginTokenUrl.split("/").filter(_.length > 0).toList,
    "LoginToken", loginTokenLocParams
  ))

  protected def loginTokenLocParams = RequireNotLoggedIn ::
    EarlyResponse(() => userMeta.handleLoginToken) :: Nil

  def buildInvitationTokenMenu = Menu(Loc(
    "InvitationToken", invitationTokenUrl.split("/").filter(_.length > 0).toList,
    "InvitationToken", invitationTokenLocParams
  ))

  protected def invitationTokenLocParams = EarlyResponse(() => userMeta.handleInvitationToken) :: Nil

  /**
   * If the Loc is in a group (or groups) like "legal" "community" etc.
   * the groups can be specified and recalled at the top level
   */
  case class LocIcon(cssIconClass: String*) extends AnyLocParam

  abstract class Sidebar(val sidebarTpl: String) extends AnyLocParam
  case object SbManagement extends Sidebar("sidebar-management")
  case object SbSettings   extends Sidebar("sidebar-settings")
  case object SbGarden     extends Sidebar("sidebar-garden")


  trait Width extends AnyLocParam
  case object FixedWidth extends Width
  case object FluidWidth extends Width

  case class ItemCss(css: String) extends AnyLocParam


}
