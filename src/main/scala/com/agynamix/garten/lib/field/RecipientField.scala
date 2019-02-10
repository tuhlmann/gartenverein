package com.agynamix.garten.lib.field

import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.mongodb.record.field.ObjectIdRefField
import com.agynamix.garten.model.User
import com.agynamix.garten.model.RecipientList
import scala.xml.Text
import net.liftweb.common._
import scala.xml.NodeSeq
import org.bson.types.ObjectId
import net.liftweb.http.SHtml
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.model.share.ClientId

abstract class RecipientField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
         extends ObjectIdRefField(rec, RecipientList) {

  OwnerType =>

//  override def displayName = "Verteiler"
//
//  /**
//   * Is this user a recipient of this element, can he view it.
//   */
//  def userIsRecipient(currentUser: User, authorId: ObjectId): Boolean =
//    obj.map(_.userIsRecipient(currentUser, authorId)) openOr false
//
//  override def asHtml = Text(obj.map(_.name.get) openOr "")
//  override def options: List[(Box[ObjectId], String)] = {
//    RecipientList.findClientLists(clientId.get) map (r => (Full(r.id.get), r.displayName))
//  }
//  override def toForm: Box[NodeSeq] = {
//    val opt = buildDisplayList.flatMap(r => r._1.map((_, r._2)))
//    Full(SHtml.selectObj[ObjectId](opt, obj.map(_.id.get), v => this.set(v)))
//  }

}
