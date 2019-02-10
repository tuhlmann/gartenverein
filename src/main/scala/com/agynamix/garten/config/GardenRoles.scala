package com.agynamix.garten.config

import com.agynamix.garten.model.User
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.CanBind
import net.liftweb.util.CssSel
import com.agynamix.garten.lib.Permission

object RolesDef {

  import Permissions._

  // These are system based roles, they have nothing todo with permissions within a client
  val R_SUPERUSER       = "superuser"
  val R_USER            = "user"

  // These roles define your technical status within a client
  val R_TEAM_SUPERVISOR = "team_supervisor"
  val R_TEAM_OWNER      = "team_owner"
  val R_TEAM_MEMBER     = "team_member"
  val R_TEAM_WATCHER    = "team_watcher"

  val R_TEAM_BOARD      = "team_board"
  val R_TEAM_FINANCING  = "team_financing"

  // Sort order definitions. To get a consistent order in which these roles appear in forms
  val SORT_SUPERUSER       = 1
  val SORT_USER            = 99

  val SORT_TEAM_SUPERVISOR = 1
  val SORT_TEAM_OWNER      = 10
  val SORT_TEAM_MEMBER     = 100
  val SORT_TEAM_WATCHER    = 110

  val SORT_TEAM_BOARD      = 20
  val SORT_TEAM_FINANCING  = 30


  val CAT_SYSTEM        = "cat_system"
  val CAT_TEAM          = "cat_team"

  lazy val P_USER            = List(UserSpace)

  lazy val P_SUPERUSER       = P_TEAM_SUPERVISOR ++ Set(SuperUser, UserSpace)

  lazy val P_TEAM_SUPERVISOR = P_TEAM_OWNER ++ Set(ClientCreate, ClientEdit, ClientDelete)

  lazy val P_TEAM_OWNER      = P_TEAM_MEMBER ++ P_TEAM_BOARD ++
                               Set(RoleCreate, RoleEdit, RoleEditAll, RoleDelete, RoleDeleteAll)

  lazy val P_TEAM_FINANCING  = P_TEAM_MEMBER ++ P_TEAM_BOARD

  lazy val P_TEAM_BOARD      = P_TEAM_MEMBER ++
                               Set(ClientView,
                                   GardenView, GardenCreate, GardenEdit, GardenEditAll, GardenDelete, GardenDeleteAll,
                                   MemberView, MemberCreate, MemberEdit, MemberEditAll, MemberDelete, MemberDeleteAll,
                                   InvoiceView, InvoiceCreate, InvoiceEdit, InvoiceEditAll,
                                   InvoiceDelete, InvoiceDeleteAll,
                                   TeamView, TeamCreate, TeamEdit, TeamEditAll, TeamDelete, TeamDeleteAll,
                                   RoleView, NoteEditAll, NoteDeleteAll, TaskEditAll, TaskDeleteAll,
                                   EventEditAll, EventDeleteAll,
                                   BlogEditAll, BlogDeleteAll,
                                   RecipientsView, RecipientsCreate, RecipientsEdit, RecipientsEditAll,
                                   RecipientsDelete, RecipientsDeleteAll,
                                   TemplateView, TemplateCreate, TemplateEdit, TemplateEditAll,
                                   TemplateDelete, TemplateDeleteAll,
                                   GenDocumentView, GenDocumentCreate, GenDocumentEdit, GenDocumentEditAll,
                                   GenDocumentDelete, GenDocumentDeleteAll,
                                   DocumentEditAll, DocumentDeleteAll)


//  lazy val P_TEAM_MEMBER     = P_TEAM_WATCHER ++ Set(GardenCreate, GardenEdit, GardenDelete,
//                               MemberCreate, MemberEdit, MemberDelete,
//                               InvoiceCreate, InvoiceEdit, InvoiceDelete, NoteCreate, NoteEdit, NoteDelete)

  // A member really has no permissions per se, only those needed for technical reasons.
  // all app specific permissions are added through other mechanisms.
  lazy val P_TEAM_MEMBER    = Set(UserSpace,
                                  ClientView,
                                  NoteView, NoteCreate, NoteEdit, NoteDelete,
                                  TaskView, TaskCreate, TaskEdit, TaskDelete,
                                  EventView, EventCreate, EventEdit, EventDelete,
                                  BlogView, BlogCreate, BlogEdit, BlogDelete,
                                  DocumentView, DocumentCreate, DocumentEdit, DocumentDelete)

  lazy val P_TEAM_WATCHER   = Set(UserSpace,
                                  ClientView, GardenView, MemberView,
                                  NoteView, NoteCreate, NoteEdit, NoteDelete,
                                  TaskView, TaskCreate, TaskEdit, TaskDelete,
                                  EventView, EventCreate, EventEdit, EventDelete,
                                  BlogView )

}

object Permissions {

  trait ToPermission {
    def hasPermission: Boolean
  }

  class BoolPerm(f: =>Boolean) extends ToPermission {
    def hasPermission = f
  }

  class PermissionPerm(p: Permission) extends ToPermission {
    def hasPermission = User.hasPermission(p)
  }

  implicit def perm2ToPermission(p: Permission) = new PermissionPerm(p)
  implicit def bool2ToPermission(b: =>Boolean) = new BoolPerm(b)
  implicit def perms2ToPermission(p: Seq[Permission]) = p.map(new PermissionPerm(_))


  def withPerm[T](elemSel: String, perm: ToPermission*)(block: => T)(implicit computer: CanBind[T]): CssSel = {
    if (perm.foldLeft(true)(_ && _.hasPermission)) {
      s"${elemSel} [onclick]" #> block
    } else {
      elemSel #> ""
    }
  }

  def withElsePerm(perm: ToPermission*)(block: => CssSel)(elseBlock: => CssSel): CssSel = {
    if (perm.foldLeft(true)(_ && _.hasPermission)) {
      block
    } else {
      elseBlock
    }
  }

  val ACTION_RO             = "view"
  val ACTION_CREATE         = "create"
  val ACTION_EDIT           = "edit"
  val ACTION_EDIT_ALL       = "edit_all"
  val ACTION_DELETE         = "delete"
  val ACTION_DELETE_ALL     = "delete_all"

  lazy val UserSpace        = Permission("user")

  lazy val SuperUser        = Permission("superuser")

  lazy val ClientAll        = Permission("client")
  lazy val ClientView       = Permission("client", ACTION_RO)
  lazy val ClientCreate     = Permission("client", ACTION_CREATE)
  lazy val ClientEdit       = Permission("client", ACTION_EDIT)
  lazy val ClientEditAll    = Permission("client", ACTION_EDIT_ALL)
  lazy val ClientDelete     = Permission("client", ACTION_DELETE)
  lazy val ClientDeleteAll  = Permission("client", ACTION_DELETE_ALL)

  lazy val TeamAll          = Permission("team")
  lazy val TeamView         = Permission("team", ACTION_RO)
  lazy val TeamCreate       = Permission("team", ACTION_CREATE)
  lazy val TeamEdit         = Permission("team", ACTION_EDIT)
  lazy val TeamEditAll      = Permission("team", ACTION_EDIT_ALL)
  lazy val TeamDelete       = Permission("team", ACTION_DELETE)
  lazy val TeamDeleteAll    = Permission("team", ACTION_DELETE_ALL)

  lazy val RoleAll          = Permission("member_role")
  lazy val RoleView         = Permission("member_role", ACTION_RO)
  lazy val RoleCreate       = Permission("member_role", ACTION_CREATE)
  lazy val RoleEdit         = Permission("member_role", ACTION_EDIT)
  lazy val RoleEditAll      = Permission("member_role", ACTION_EDIT_ALL)
  lazy val RoleDelete       = Permission("member_role", ACTION_DELETE)
  lazy val RoleDeleteAll    = Permission("member_role", ACTION_DELETE_ALL)

  lazy val RecipientsAll       = Permission("recipients")
  lazy val RecipientsView      = Permission("recipients", ACTION_RO)
  lazy val RecipientsCreate    = Permission("recipients", ACTION_CREATE)
  lazy val RecipientsEdit      = Permission("recipients", ACTION_EDIT)
  lazy val RecipientsEditAll   = Permission("recipients", ACTION_EDIT_ALL)
  lazy val RecipientsDelete    = Permission("recipients", ACTION_DELETE)
  lazy val RecipientsDeleteAll = Permission("recipients", ACTION_DELETE_ALL)

  lazy val GardenAll        = Permission("garden")
  lazy val GardenView       = Permission("garden", ACTION_RO)
  lazy val GardenCreate     = Permission("garden", ACTION_CREATE)
  lazy val GardenEdit       = Permission("garden", ACTION_EDIT)
  lazy val GardenEditAll    = Permission("garden", ACTION_EDIT_ALL)
  lazy val GardenDelete     = Permission("garden", ACTION_DELETE)
  lazy val GardenDeleteAll  = Permission("garden", ACTION_DELETE_ALL)

  lazy val MemberAll        = Permission("member")
  lazy val MemberView       = Permission("member", ACTION_RO)
  lazy val MemberCreate     = Permission("member", ACTION_CREATE)
  lazy val MemberEdit       = Permission("member", ACTION_EDIT)
  lazy val MemberEditAll    = Permission("member", ACTION_EDIT_ALL)
  lazy val MemberDelete     = Permission("member", ACTION_DELETE)
  lazy val MemberDeleteAll  = Permission("member", ACTION_DELETE_ALL)

  lazy val InvoiceAll       = Permission("invoice")
  lazy val InvoiceView      = Permission("invoice", ACTION_RO)
  lazy val InvoiceCreate    = Permission("invoice", ACTION_CREATE)
  lazy val InvoiceEdit      = Permission("invoice", ACTION_EDIT)
  lazy val InvoiceEditAll   = Permission("invoice", ACTION_EDIT_ALL)
  lazy val InvoiceDelete    = Permission("invoice", ACTION_DELETE)
  lazy val InvoiceDeleteAll = Permission("invoice", ACTION_DELETE_ALL)

  lazy val TemplateAll       = Permission("template")
  lazy val TemplateView      = Permission("template", ACTION_RO)
  lazy val TemplateCreate    = Permission("template", ACTION_CREATE)
  lazy val TemplateEdit      = Permission("template", ACTION_EDIT)
  lazy val TemplateEditAll   = Permission("template", ACTION_EDIT_ALL)
  lazy val TemplateDelete    = Permission("template", ACTION_DELETE)
  lazy val TemplateDeleteAll = Permission("template", ACTION_DELETE_ALL)

  lazy val NoteAll          = Permission("note")
  lazy val NoteView         = Permission("note", ACTION_RO)
  lazy val NoteCreate       = Permission("note", ACTION_CREATE)
  lazy val NoteEdit         = Permission("note", ACTION_EDIT)
  lazy val NoteEditAll      = Permission("note", ACTION_EDIT_ALL)
  lazy val NoteDelete       = Permission("note", ACTION_DELETE)
  lazy val NoteDeleteAll    = Permission("note", ACTION_DELETE_ALL)

  lazy val TaskAll       = Permission("task")
  lazy val TaskView      = Permission("task", ACTION_RO)
  lazy val TaskCreate    = Permission("task", ACTION_CREATE)
  lazy val TaskEdit      = Permission("task", ACTION_EDIT)
  lazy val TaskEditAll   = Permission("task", ACTION_EDIT_ALL)
  lazy val TaskDelete    = Permission("task", ACTION_DELETE)
  lazy val TaskDeleteAll = Permission("task", ACTION_DELETE_ALL)

  lazy val EventAll       = Permission("event")
  lazy val EventView      = Permission("event", ACTION_RO)
  lazy val EventCreate    = Permission("event", ACTION_CREATE)
  lazy val EventEdit      = Permission("event", ACTION_EDIT)
  lazy val EventEditAll   = Permission("event", ACTION_EDIT_ALL)
  lazy val EventDelete    = Permission("event", ACTION_DELETE)
  lazy val EventDeleteAll = Permission("event", ACTION_DELETE_ALL)

  lazy val DocumentAll       = Permission("document")
  lazy val DocumentView      = Permission("document", ACTION_RO)
  lazy val DocumentCreate    = Permission("document", ACTION_CREATE)
  lazy val DocumentEdit      = Permission("document", ACTION_EDIT)
  lazy val DocumentEditAll   = Permission("document", ACTION_EDIT_ALL)
  lazy val DocumentDelete    = Permission("document", ACTION_DELETE)
  lazy val DocumentDeleteAll = Permission("document", ACTION_DELETE_ALL)

  lazy val GenDocumentAll       = Permission("gen_document")
  lazy val GenDocumentView      = Permission("gen_document", ACTION_RO)
  lazy val GenDocumentCreate    = Permission("gen_document", ACTION_CREATE)
  lazy val GenDocumentEdit      = Permission("gen_document", ACTION_EDIT)
  lazy val GenDocumentEditAll   = Permission("gen_document", ACTION_EDIT_ALL)
  lazy val GenDocumentDelete    = Permission("gen_document", ACTION_DELETE)
  lazy val GenDocumentDeleteAll = Permission("gen_document", ACTION_DELETE_ALL)

  lazy val BlogAll        = Permission("blog")
  lazy val BlogView       = Permission("blog", ACTION_RO)
  lazy val BlogCreate     = Permission("blog", ACTION_CREATE)
  lazy val BlogEdit       = Permission("blog", ACTION_EDIT)
  lazy val BlogEditAll    = Permission("blog", ACTION_EDIT_ALL)
  lazy val BlogDelete     = Permission("blog", ACTION_DELETE)
  lazy val BlogDeleteAll  = Permission("blog", ACTION_DELETE_ALL)

}

object ApplicationCategories extends Enumeration {
  type ApplicationCategory = Value

  val System = Value("cat_system")
  val Team   = Value("cat_team")

}
