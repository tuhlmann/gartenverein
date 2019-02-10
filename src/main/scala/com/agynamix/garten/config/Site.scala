package com.agynamix.garten
package config

import net.liftweb._
import common._
import http.S
import sitemap._
import sitemap.Loc._
import scala.xml.Text
import com.agynamix.garten.model.User
import com.agynamix.garten.snippet._
import net.liftweb.http.LiftRules
import com.agynamix.garten.config.MenuGroups._
import net.liftweb.sitemap.Menu
import net.liftweb.http.S
import net.liftweb.sitemap.Loc.LocGroup
import com.agynamix.garten.model.User
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Loc._
import com.agynamix.garten.lib.Locs._
import com.agynamix.garten.lib.Locs.ItemCss
import net.liftweb.sitemap.Loc.LocGroup
import com.agynamix.garten.lib.Locs.LocIcon

object MenuGroups {
  val SettingsGroup = LocGroup("settings")
  val TopBarGroup   = LocGroup("topbar")
}

/**
 * Wrapper for Menu locations
 */
case class MenuLoc(menu: Menu) {
  lazy val url: String     = S.contextPath+menu.loc.calcDefaultHref
  lazy val fullUrl: String = S.hostAndPath+menu.loc.calcDefaultHref
}

object Site {

  import MenuGroups._

  val home            = MenuLoc(Menu("Home", "Start") / "index" >> LocIcon("icon-home", "icon-white") >> RedirectToDashBoardWhenLoggedIn >> RequireNotLoggedIn >> FixedWidth)
  val loginToken      = MenuLoc(buildLoginTokenMenu)
  val invitationToken = MenuLoc(buildInvitationTokenMenu)
  val login           = MenuLoc(Menu("Login", "Anmeldung") / "login" >> RequireNotLoggedIn >> FixedWidth)
  val logout          = MenuLoc(buildLogoutMenu)
  protected val profileParamMenu = Menu.param[User]("User", "Profile", User.findByEmail _, _.email.get) /
                                 "user" >> Loc.CalcValue(() => User.currentUser) >> SbSettings >> RequireLoggedIn
  lazy val profileLoc  = profileParamMenu.toLoc

  val reqInvite       = MenuLoc(Menu("ReqInvite", "Einladung") / "req_invite" >> RequireNotLoggedIn >> Hidden)
  val resetPassword   = MenuLoc(Menu("ResetPassword", "Neues Passwort") / "set_password" >> Hidden >> RequireNotLoggedIn)

  val password        = MenuLoc(Menu("Password", "Passwort") / "settings" / "password" >> RequireLoggedIn >> SbSettings >> SettingsGroup)
  val twoFactorAuth   = MenuLoc(Menu("TwoFactorAuth", "Zwei-Faktor Authentifizierung") / "settings" / "two-factor-auth" >> RequireLoggedIn >> SbSettings >> SettingsGroup)
  val account         = MenuLoc(Menu("Account", "Anmeldeinformationen") / "settings" / "account" >> SettingsGroup >> RequireLoggedIn >> SbSettings)
  val editProfile      = MenuLoc(Menu("EditProfile", "Profil") / "settings" / "profile" >> SettingsGroup >> RequireLoggedIn >> SbSettings)
  val clients         = MenuLoc(Menu("Clients", "Mandanten") / "settings" / "clients" >> SettingsGroup >> RequireLoggedIn >> SbSettings)
  val register        = MenuLoc(Menu("Register", "Registrieren") / "register" >> RequireNotLoggedIn)
  //val team            = MenuLoc(Menu("Team", "Team") / "management" / "team" >> SettingsGroup >> RequireLoggedIn >> RequireHasClient >> SbManagement >> HasPermission(Permissions.TeamView))
  val memberRole      = MenuLoc(Menu("MemberRoles", "Rolle") / "management" / "roles" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.RoleView))
  val recipients      = MenuLoc(Menu("Recipients", "Verteiler") / "management" / "recipients" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.RecipientsView))
  val memberTypes     = MenuLoc(Menu("MemberTypes", "Art der Mitgliedschaft") / "management" / "member_types" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.RoleView))
  val memberPositions = MenuLoc(Menu("MemberPositions", "Rolle im Verein") / "management" / "member_positions" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.RoleView))
  val propertyTaxes   = MenuLoc(Menu("PropertyTaxes", "Grundsteuer") / "management" / "property_taxes" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.RoleView))
  val propertyValues  = MenuLoc(Menu("PropertyValues", "Schlüssel und Werte") / "management" / "property_values" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.InvoiceView))


  val documentTemplates  = MenuLoc(Menu("DocumentTemplates", "Vorlagen") / "management" / "templates" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.TemplateView))
  val generatedDocuments = MenuLoc(Menu("GeneratedDocuments", "Generierte Dokumente") / "management" / "documents" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.GenDocumentView))
  val documentLibrary    = MenuLoc(Menu("DocumentFolders", "Dokumentenmappen") / "management" / "library" >> SettingsGroup >> RequireLoggedIn >> SbManagement >> RequireHasClient >> HasPermission(Permissions.DocumentView))
  
  // Invoicing
  val invoices        = MenuLoc(Menu("Invoices", "Rechnungen") / "invoicing" / "invoices" >> RequireLoggedIn >> Hidden >> RequireHasClient >> SbManagement >> HasPermission(Permissions.InvoiceView) >> LocIcon("icon-inbox", "icon-white"))
  val vat             = MenuLoc(Menu("VAT", "Mehrwertsteuer") / "invoicing" / "vat" >> RequireLoggedIn >> Hidden >> RequireHasClient >> SbManagement >> HasPermission(Permissions.InvoiceView) >> LocIcon("icon-money", "icon-white"))
  val numberRanges    = MenuLoc(Menu("NumberRanges", "Nummernkreise") / "invoicing" / "number-ranges" >> RequireLoggedIn >> SbManagement >> Hidden >> RequireHasClient >> HasPermission(Permissions.InvoiceView) >> LocIcon("icon-money", "icon-white"))
  val textBlocks      = MenuLoc(Menu("TextBlocks", "Textblöcke") / "invoicing" / "text-blocks" >> RequireLoggedIn >> SbManagement >> Hidden >> RequireHasClient >> HasPermission(Permissions.InvoiceView) >> LocIcon("icon-money", "icon-white"))
  val invoiceArticles = MenuLoc(Menu("InvoiceArticles", "Rechungsartikel") / "invoicing" / "articles" >> RequireLoggedIn >> SbManagement >> RequireHasClient >> Hidden >> HasPermission(Permissions.InvoiceView) >> LocIcon("icon-money", "icon-white"))

  // Menu entries for logged in users
  val dashboard       = MenuLoc(Menu("Dashboard", "Cockpit") / "dashboard" >> RequireLoggedIn >> Hidden >> LocIcon("icon-dashboard", "icon-white") >> SbManagement)

  // Admin pages, only for SuperUsers
  val userAccounts       = MenuLoc(Menu("UserAccounts", "Anwender verwalten") / "admin" / "user_accounts" >> RequireLoggedIn >> SbManagement >> RequireSuperUser >> Hidden >> LocIcon("icon-dashboard", "icon-white"))
  val clientsAndAccounts = MenuLoc(Menu("ClientsAndAccounts", "Vereine verwalten") / "admin" / "clients_and_accounts" >> RequireLoggedIn >> SbManagement >> RequireSuperUser >> Hidden >> LocIcon("icon-dashboard", "icon-white"))

  // locations (menu entries)
  //val home = MenuLoc(Menu("Home", "Start") / "index" >> TopBarGroup >> LocIcon("icon-home", "icon-white") >> RedirectToDashBoardWhenLoggedIn >> Area(ApplicationArea.Public)) // >> RequireNotLoggedIn
  val gardens         = MenuLoc(Menu("Garden", "Gärten") / "gardens" >> RequireLoggedIn >> RequireHasClient >> SbManagement >> HasPermission(Permissions.GardenView) >> Hidden >> LocIcon("icon-fire", "icon-white"))
  val members         = MenuLoc(Menu("Members", "Mitglieder") / "members" >> RequireLoggedIn >> Hidden >> RequireHasClient >> SbManagement >> HasPermission(Permissions.MemberView) >> LocIcon("icon-group", "icon-white"))

  val notes           = MenuLoc(Menu("Notes", "Notizen") / "notes" >> RequireLoggedIn >> Hidden >> RequireHasClient >> SbManagement >> HasPermission(Permissions.NoteView) >> LocIcon("icon-pencil", "icon-white"))
  val tasks           = MenuLoc(Menu("Tasks", "Aufgaben") / "tasks" >> RequireLoggedIn >> Hidden >> RequireHasClient >> SbManagement >> HasPermission(Permissions.TaskView) >> LocIcon("icon-pencil", "icon-white"))
  val events          = MenuLoc(Menu("Events", "Termine") / "events" >> RequireLoggedIn >> Hidden >> RequireHasClient >> SbManagement >> HasPermission(Permissions.EventView) >> LocIcon("icon-pencil", "icon-white"))
  val myDocuments     = MenuLoc(Menu("MyDocuments", "Dokumente") / "my_documents" >> RequireLoggedIn >> Hidden >> RequireHasClient >> SbManagement >> HasPermission(Permissions.DocumentView) >> LocIcon("icon-pencil", "icon-white"))
  val myMembership    = MenuLoc(Menu("MyMembership", "Meine Mitgliedschaft") / "settings" / "my_membership" >> RequireLoggedIn >> Hidden >> RequireHasClient >> SbSettings >> LocIcon("icon-pencil", "icon-white"))

  // Claim your account
  val claim           = MenuLoc(Menu("Claim", "Zugang einrichten") / "claim" >> RequireNotLoggedIn >> Hidden >> LocIcon("icon-group", "icon-white"))

  val blog            = MenuLoc(Menu("Blog", "Blog") / "blog" >> TopBarGroup >> LocIcon("icon-pencil", "icon-white") >> FixedWidth)
  val blogManager     = MenuLoc(Menu("BlogManager", "Blog Manager") / "blogmanager" >> RequireLoggedIn >> Hidden >> RequireSuperUser >> RequireHasClient >> SbManagement >> LocIcon("icon-pencil", "icon-white"))

  val contact         = MenuLoc(Menu("Contact", "Kontakt") / "contact" >> TopBarGroup >> LocIcon("icon-info-sign", "icon-white") >> FixedWidth)
  val imprint         = MenuLoc(Menu("Imprint", "Impressum") / "imprint" >> TopBarGroup >> LocIcon("icon-legal", "icon-white") >> FixedWidth >> ItemCss("visible-large"))

  // Actions
  val meterReadings   = MenuLoc(Menu("MeterReadings", "Zählerstände") / "actions" / "yearly_readings" >> RequireLoggedIn >> RequireHasClient >> SbManagement >> HasPermission(Permissions.GardenEditAll) >> Hidden >> LocIcon("icon-fire", "icon-white"))

  lazy val menus = List(
    home.menu,
    login.menu,
    register.menu,
    resetPassword.menu,
    loginToken.menu,
    invitationToken.menu,
    logout.menu,
    reqInvite.menu,
    claim.menu,
    profileParamMenu,
    account.menu,
    password.menu,
    twoFactorAuth.menu,
    editProfile.menu,
    clients.menu,
    /*team.menu,*/
    memberRole.menu,
    recipients.menu,
    memberTypes.menu,
    memberPositions.menu,
    propertyTaxes.menu,
    propertyValues.menu,
    documentTemplates.menu,
    generatedDocuments.menu,
    documentLibrary.menu,
    dashboard.menu,
    gardens.menu,
    members.menu,
    notes.menu,
    tasks.menu,
    events.menu,
    myDocuments.menu,
    myMembership.menu,
    blogManager.menu,
    vat.menu,
    numberRanges.menu,
    textBlocks.menu,
    invoiceArticles.menu,
    invoices.menu,
    userAccounts.menu,
    clientsAndAccounts.menu,
    ClientsAndAccounts.listMenuParam,
    meterReadings.menu,
    Menu("About", "Über") / "about" >> TopBarGroup >> LocIcon("icon-asterisk", "icon-white") >> FixedWidth,
    contact.menu,
    imprint.menu,
    blog.menu,
    Menu.i("Throw") / "throw" >> Hidden,
    Menu.i("Error") / "error" >> Hidden,
    Menu.i("404") / "404" >> Hidden,
    Menu.i("Test") / "test" >> Hidden,
    Memberships.listMenu,
    Gardens.listMenu,
    Gardens.invoicesPerGardenMenu,
    Notes.listMenuParam,
    Events.listMenuParam,
    Tasks.listMenuParam,
    BlogManager.listMenuParam,
    MyDocuments.listMenuParam,
    DocumentFolders.listMenuParam
  )

  /*
   * Return a SiteMap needed for Lift
   */
  def siteMap: SiteMap = SiteMap(menus:_*)

  def siteResourceNames = "i18n/lift-garten"

}
