package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.share._
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.mongodb.BsonDSL._
import com.agynamix.garten.lib.field._
import org.bson.types.ObjectId
import net.liftweb.http.S
import com.agynamix.garten.lib.util.LocalizedEnum
import net.liftweb.json.JsonAST.JObject
import net.liftweb.mongodb.record.field.ObjectIdField


object LoggedAction extends Enumeration with LocalizedEnum {
  type LoggedAction = Value

  val locKeyBase = "logbook.action"

  val Unknown            = Value(0, "unknown")
  val Create             = Value(1, "create")
  val Read               = Value(2, "read") // probably not used
  val Update             = Value(3, "update")
  val Delete             = Value(4, "delete")
  val Login              = Value(5, "login")
  val Logout             = Value(6, "logout")
  val InvalidCredentials = Value(7, "invalid")
}

//object RecordEntityType extends Enumeration with LocalizedEnum {
//  type RecordEntityType = Value
//
//  val locKeyBase = "record.entity.type"
//
//  val User = Value(0, "user")
//  val Membership = Value(1, "membership")
//  val Garden     = Value(2, "garden")
//
//}

object Logbook extends Logbook with MongoMetaRecord[Logbook] with StructuredDbObject[Logbook] {

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>(clientId.name -> id))

  def createInstance(userId: ObjectId, clientId: ObjectId, remoteAddr: String, action: LoggedAction.Value): Logbook =
    Logbook.createRecord.userId(userId).clientId(clientId).remoteAddr(remoteAddr).action(action)

  def logUserLoggedIn(user: User): Unit = logEvent(user, LoggedAction.Login)

  def logUserLoggedOut(user: User): Unit = logEvent(user, LoggedAction.Logout)

  def logInvalidLoginAttempt(email: String): Unit =
    User.findByEmail(email).foreach(user => logEvent(user, LoggedAction.InvalidCredentials))

  def logCreateRecord(rec: MongoIdRecord[_]): Unit = logEvent(rec, LoggedAction.Create)

  def logUpdateRecord(rec: MongoIdRecord[_]): Unit = logEvent(rec, LoggedAction.Update)

  def logDeleteRecord(rec: MongoIdRecord[_]): Unit = logEvent(rec, LoggedAction.Delete)

  def logEvent(user: User, action: LoggedAction.Value): Unit = {
    for {clientId <- user.activeMembership.clientId
         req <- S.request } {
      createInstance(user.id.get, clientId,
                     req.request.header("X-Real-IP").openOr(req.request.remoteAddress), action).save(true)
    }
  }

  def logEvent(rec: MongoIdRecord[_], action: LoggedAction.Value): Unit = {
    for {user <- User.currentUser
         clientId <- user.activeMembership.clientId
         req <- S.request } {
      val inst = createInstance(user.id.get, clientId, 
                                req.request.header("X-Real-IP").openOr(req.request.remoteAddress), action)
      inst.accessedObject(rec.id.get)
      inst.accessedObjectType(rec.getClass.getName)
      // Log the record
      inst.save(true)
    }
  }

}

class Logbook private() extends MongoIdRecord[Logbook] with ClientId[Logbook] with UserId[Logbook] with Created[Logbook] {
  def meta = Logbook

  object remoteAddr extends BsStringField(this, 30) {
    override def displayName = "IP Adresse"
  }

  object action extends BsEnumNameField(this, LoggedAction) {
    override def displayName = "Aktion"
  }

  object accessedObject extends ObjectIdField(this) {
    override def optional_? = true
  }

  object accessedObjectType extends BsStringField(this, 200)
  // Also: accessedObject: ObjectId, and a descriptor (enum, etc) what type of object was accessed

}

