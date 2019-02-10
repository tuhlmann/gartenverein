package com.agynamix.garten.lib.field

import com.agynamix.invoice.model.Vat
import net.liftweb.common._
import net.liftweb.util.Helpers._
import org.bson.types.ObjectId
import net.liftweb.mongodb.record.BsonRecord
import scala.xml.NodeSeq
import scala.xml.Text
import net.liftweb.http.SHtml
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.model.Client

abstract class BsVatField[OwnerType <: BsonRecord[OwnerType] ](rec: OwnerType)
               extends BsObjectIdRefField(rec, Vat) {

  def getClientId: ObjectId

  override def options: List[(Box[ObjectId], String)] = {
    Vat.findClientVats(getClientId) map (v => (Full(v.id.get), v.name.get))
  }
  override def toForm: Box[NodeSeq] = {
   val opt = buildDisplayList.flatMap(r => r._1.map((_, r._2)))
    Full(SHtml.selectObj[ObjectId](opt, obj.map(_.id.get), (v: ObjectId) => { this.set(v) }, "class" -> "form-control"))
  }

  def vatAsHtml = {
    (for (vat <- obj) yield {
      Text(""+vat.vat.get+"%")
    }) openOr Text("")
  }

  override def asHtml = obj.map(_.vat.asHtml).openOr(NodeSeq.Empty)

}
