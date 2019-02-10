package com.agynamix.garten.lib

import net.liftweb.common._
import net.liftweb.mongodb.record.MongoRecord
import com.agynamix.garten.model.User
import net.liftweb.json.JsonAST.JObject
import net.liftweb.mongodb.record.MongoMetaRecord
import com.mongodb.DBObject
import net.liftweb.mongodb.FindOption
import net.liftweb.mongodb.MongoMeta
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.model.Client

trait StructuredDbObject[BaseRecord <: MongoIdRecord[BaseRecord]] extends MongoMetaRecord[BaseRecord] {

  self: BaseRecord =>

  def keyFields(user: User, args: Any*): Box[()=>JObject]

}