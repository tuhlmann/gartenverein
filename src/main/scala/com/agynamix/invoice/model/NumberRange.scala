package com.agynamix.invoice.model

import net.liftweb.common._
import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.model.share.UserId
import com.agynamix.garten.model.User
import net.liftweb.common._
import scala.xml.UnprefixedAttribute
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.share.CreatedUpdated
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.record.field.StringField
import net.liftweb.record.field.IntField
import com.foursquare.rogue.LiftRogue._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.json.JsonAST.JObject
import com.agynamix.garten.model.share.FormValidators
import com.agynamix.garten.model.Client
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsIntField
import org.bson.types.ObjectId


object NumberRangeEnum extends Enumeration {
  type NumberRangeEnum = Value
  val Invoices         = Value("Invoice")
}

object NumberRange extends NumberRange with MongoMetaRecord[NumberRange] with StructuredDbObject[NumberRange] with FormValidators[NumberRange] {

  val DEF_FORMAT_STR = "%s"

//  def keyFields2(user: User, args: Any*): (String, List[Any]) =
//    ("%s = ? and %s = ?".format(userId.name, clientId.name), List(user.id.is, user.activeClientConnection.clientId))

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>(userId.name -> user.id.get) ~ (clientId.name -> id))

  def createInstance(user: User, client: Client) = NumberRange.createRecord.userId(user.id.get).clientId(client.id.get)

  def checkOrInitRanges(user: User): Unit = NumberRangeEnum.values.foreach(value => checkOrInitRange(user, value))

  def checkOrInitRange(userBox: Box[User], range: NumberRangeEnum.Value): Box[NumberRange] = {
    userBox.flatMap(u => checkOrInitRange(u, range))
  }

  def checkOrInitRange(user: User, range: NumberRangeEnum.Value): Box[NumberRange] = {
    for (clientId <- user.activeMembership.clientId) yield {
      //println("Search Range: "+range)
      val re = (NumberRange where (_.userId eqs user.id.get) and (_.clientId eqs clientId) and (_.identifier eqs range) get())
      re.getOrElse{
        println("Create new Range "+range)
        createDefault(user, range).save(true)
      }
    }
  }

  def createDefault(user: User, range: NumberRangeEnum.Value): NumberRange =
    NumberRange.createRecord.userId(user.id.get).clientId(user.activeMembership.clientId).identifier(range).formatStr(DEF_FORMAT_STR).counter(1)

  def atomicNextValue(rangeId: ObjectId): String = {
/*
    ScheduledTask where (_.dueTime before timeNow) and (_.taskState eqs ScheduledTaskState.Open) findAndModify
      (_.taskState setTo ScheduledTaskState.InProgress) and (_.inProgressStart setTo timeNow) updateOne (true)
 */
    (for (range <- NumberRange where (_.id eqs rangeId) findAndModify (_.counter inc 1) updateOne(true)) yield {
      range.formatStr.formattedValue
    }) getOrElse ""
    //format_str.formattedValue
  }

}

class NumberRange private() extends MongoIdRecord[NumberRange] with UserId[NumberRange] with ClientId[NumberRange] with CreatedUpdated[NumberRange] {
  def meta = NumberRange

  object identifier extends BsEnumNameField(this, NumberRangeEnum) {
    override def displayName = "Bezeichnung"
    //override def toFormAppendedAttributes = new UnprefixedAttribute("disabled", "disabled", super.toFormAppendedAttributes)

    /**
     * Get a.toString through S ? to localize it: invoice.numberRange.enum.Invoice = Rechnung
     */
    override def buildDisplayList: List[(Box[NumberRangeEnum.Value], String)] =
      enum.values.toList.map(a => (Full(a), a.toString))

  }

  object formatStr extends BsStringField(this, 50) {
    override def displayName = "Format String"
    override def get = {
      val v = super.get
      if (v.indexOf("%s") > -1) v else v + "%s"
    }
    def formattedValue = get.format(counter.get)
  }

  object counter extends BsIntField(this) {
    override def displayName = "Zähler"
  }


}