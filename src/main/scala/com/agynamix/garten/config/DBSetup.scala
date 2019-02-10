package com.agynamix.garten.config

import net.liftweb.common.Loggable
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.model.Client
import com.agynamix.garten.model.RecipientList
import com.agynamix.garten.model.Role
import com.agynamix.invoice.model.Vat
import com.agynamix.garten.model.SystemUser
import com.agynamix.garten.model.MemberType
import com.agynamix.garten.model.MemberPosition
import com.agynamix.garten.model.DocumentType
import com.agynamix.garten.model.Document
import net.liftweb.common._
import com.agynamix.garten.model.DocumentTemplate
import com.agynamix.garten.model.DocumentResource
import com.agynamix.garten.model.User
import com.agynamix.garten.model.PropertyTax

object DBSetup extends Loggable {

  import Permissions._
  import RolesDef._

  def run() {
    try {
      //clearDefaultRoles // disable once stable
      setupStandardRoles
      setupStandardVats
      setupStandardMemberTypes
      setupStandardPropertyTaxes
      setupStandardRecipientLists
      ensureSystemUserSetup
      ensureDocumentsUploaded
    } catch {
      case e: Throwable => logCause(e)
    }
  }

  private def logCause(t: Throwable): Unit = {
    logger.error(t.getStackTraceString)
    if (t.getCause() != null) logCause(t.getCause())
  }

  def clearDefaultRoles = {
    (Role where (_.defaultRole eqs true) fetch).foreach(_.delete_!)
  }

  def ensureSystemUserSetup {
    val suRole = Role.findOrCreateAndSaveSU()
    if (!SystemUser.user.roles.objs.exists(_.id.get == suRole.id.get)) {
      SystemUser.user.roles(List(suRole.id.get)).save(true)
    }
  }

  /**
   * Add system roles.
   */
  def setupStandardRoles {

    Role.findOrCreateAndSaveSU
    Role.findOrCreateAndSaveDefault(R_USER            , CAT_SYSTEM   , SORT_USER           , P_USER.toSeq            :_*)
    Role.findOrCreateAndSaveDefault(R_TEAM_SUPERVISOR , CAT_TEAM     , SORT_TEAM_SUPERVISOR, P_TEAM_SUPERVISOR.toSeq :_*)

    Role.findOrCreateAndSaveDefault(R_TEAM_OWNER      , CAT_TEAM     , SORT_TEAM_OWNER     , P_TEAM_OWNER.toSeq      :_*)
    Role.findOrCreateAndSaveDefault(R_TEAM_BOARD      , CAT_TEAM     , SORT_TEAM_BOARD     , P_TEAM_BOARD.toSeq      :_*)
    Role.findOrCreateAndSaveDefault(R_TEAM_FINANCING  , CAT_TEAM     , SORT_TEAM_FINANCING , P_TEAM_FINANCING.toSeq  :_*)
    Role.findOrCreateAndSaveDefault(R_TEAM_MEMBER     , CAT_TEAM     , SORT_TEAM_MEMBER    , P_TEAM_MEMBER.toSeq     :_*)
    Role.findOrCreateAndSaveDefault(R_TEAM_WATCHER    , CAT_TEAM     , SORT_TEAM_WATCHER   , P_TEAM_WATCHER.toSeq   :_*)

    // Check for every client that a set of roles has been copied. This should be removed once
    // the role setup is stable
    Client.findAll.foreach( client => Role.findOrCopyClientRoles(client.id.get) )
  }

  def setupStandardVats {
	  Client.findAll.foreach( Vat.findOrCreateStandardVats )
  }

  def setupStandardMemberTypes {
    Client.findAll.foreach( MemberType.findOrCreateStandardTypes )
  }

  def setupStandardPropertyTaxes {
    Client.findAll.foreach( PropertyTax.findOrCreateStandardTaxes )
  }

  def setupStandardRecipientLists {
    // doesn't currently wotk because S is not available (for localization)
//    Client.findAll.foreach(client => {
//      Client.findAllOwners(client.id.get).headOption.map(owner => {
//        RecipientList.findOrCreateDefaults(owner, client)
//      })
//    })
  }

  def setupStandardMemberPositions {
    Client.findAll.foreach( MemberPosition.findOrCreateStandardPositions )
  }


  /*
   * We do deliver some documents inside the application archive
   * These documents need to be uploaded into the common file structure
   * and eventually be updated if new versions arrive
   */
  def ensureDocumentsUploaded {
    Client.findAll.foreach(client => {
      Client.findAllOwners(client.id.get).headOption.map(owner => {
        DocumentTemplate.ensureStdDocumentUploadForClient(owner, client)
      })
    })
  }

}