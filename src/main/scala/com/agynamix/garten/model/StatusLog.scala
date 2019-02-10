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
import net.liftweb.json.JsonAST.{JArray, JNothing, JObject}
import net.liftweb.mongodb.record.field.ObjectIdField
import com.foursquare.rogue.LiftRogue._


object StatusLogKey extends Enumeration with LocalizedEnum {
  type StatusLogKey = Value

  val locKeyBase = "statuslog.key"

  val Unknown            = Value(0, "unknown")
  val SchedulerRun       = Value(1, "scheduler.run")
}

object StatusLogReturnCode extends Enumeration with LocalizedEnum {
  type StatusLogReturnCode = Value

  val locKeyBase = "statuslog.returncode"

  val Ok                 = Value(0, "ok")
  val Error              = Value(1, "error")
  val ErrorWithException = Value(2, "error_exception")
}

object StatusLogAction extends Enumeration with LocalizedEnum {
  type StatusLogAction = Value

  val locKeyBase = "statuslog.action"

  val Unknown            = Value(0, "unknown")
}

object StatusLog extends StatusLog with MongoMetaRecord[StatusLog] with StructuredDbObject[StatusLog] {

  ensureIndex((key.name -> 1), true)

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    Full(()=>JObject(Nil))

  def logSchedulerJob(rc: StatusLogReturnCode.Value, processedRecs: Int): Unit = {
    StatusLog where (_.key eqs StatusLogKey.SchedulerRun) bulkDelete_!!!

    StatusLog.createRecord
             .key(StatusLogKey.SchedulerRun)
             .rc(rc)
             .processedRecords(processedRecs)
             .save(true)
  }

}

class StatusLog private() extends MongoIdRecord[StatusLog] with Created[StatusLog] {
  def meta = StatusLog

  object key extends BsEnumNameField(this, StatusLogKey) {
    override def displayName = "Schlüssel"
  }

  object rc extends BsEnumNameField(this, StatusLogReturnCode) {
    override def displayName = "Rückgabewert"
  }

  object processedRecords extends BsIntField(this) {
    override def displayName = "Verarbeite Datensätze"
  }

}

