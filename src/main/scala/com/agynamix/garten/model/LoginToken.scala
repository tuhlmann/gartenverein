package com.agynamix.garten
package model

import com.agynamix.garten.lib.field.ExpiresField
import org.joda.time.Hours
import net.liftweb._
import common._
import http._
import mongodb.record._
import mongodb.record.field._
import record.MandatoryTypedField
import org.bson.types.ObjectId
import com.agynamix.garten.config.GardenConfig
import java.util.UUID
import net.liftweb.util.Helpers
import com.foursquare.rogue.LiftRogue._

object LoginToken extends LoginToken with MongoMetaRecord[LoginToken] {
  import mongodb.BsonDSL._

  ensureIndex((userId.name -> 1))
  ensureIndex((invitationId.name -> 1))

  private lazy val loginTokenUrl          = GardenConfig.loginTokenUrl.vend
  private lazy val invitationTokenUrl     = GardenConfig.invitationTokenUrl.vend
  private lazy val loginTokenExpires      = GardenConfig.loginTokenExpires.vend
  private lazy val invitationTokenExpires = GardenConfig.invitationTokenExpires.vend

  def loginUrl(inst: LoginToken): String = "%s%s?token=%s".format(S.hostAndPath, loginTokenUrl, inst.id.toString)
  def invitationUrl(inst: LoginToken): String = "%s%s?token=%s".format(S.hostAndPath, invitationTokenUrl, inst.id.toString)

  def createForUserId(uid: ObjectId): LoginToken = {
    createRecord.userId(uid).save(true)
  }

  def createForTeamInvitation(userId: ObjectId, uid: ObjectId, renewIfExists: Boolean = false): LoginToken = {
    LoginToken where (_.invitationId eqs uid) get() match {
      case Some(token) if renewIfExists => token.expires(invitationTokenExpires).save(true)
      case _ => createRecord.userId(userId).invitationId(uid).expires(invitationTokenExpires).save(true)
    }
  }

  def deleteAllByUserId(uid: ObjectId) {
    delete(userId.name, uid)
  }

  def deleteAllByInvitationId(uid: ObjectId) {
    delete(invitationId.name, uid)
  }

  def findByStringId(in: String): Box[LoginToken] =
    Helpers.tryo(find(UUID.fromString(in))) openOr Empty
}

/**
  * This is a token for automatically logging a user in
  */
class LoginToken extends MongoRecord[LoginToken] with UUIDPk[LoginToken] {
  def meta = LoginToken

  /**
   * For use as a login token
   */
  object userId extends ObjectIdField(this)

  /**
   * For use as an invitation token
   */
  object invitationId extends ObjectIdField(this)

  object expires extends ExpiresField(this, meta.loginTokenExpires)

  def loginUrl: String      = meta.loginUrl(this)
  def invitationUrl: String = meta.invitationUrl(this)
}
