package com.agynamix.garten.lib

import com.agynamix.invoice.model.InvoiceContainer
import com.agynamix.garten.model.Membership
import net.liftweb.common._
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.model.GeneratedDocument
import com.agynamix.invoice.model.GeneratedInvoice
import com.agynamix.garten.model.Garden

object InvoiceContainerDataSource extends Loggable {
  def apply(invoice: InvoiceContainer): DataSource = apply(invoice, Nil: _*)
  def apply(invoice: InvoiceContainer, childSources: DataSource*): DataSource = {
    new CompositeDataSource(invoice, childSources: _*)
  }
}

object GeneratedInvoiceDataSource extends Loggable {
  def apply(generatedInvoice: GeneratedInvoice): DataSource = {
    new BsonRecordDataSource("doc", generatedInvoice)
  }
}

object MembershipDataSources extends Loggable {

  def apply(member: Membership): List[DataSource] = apply(member, Nil: _*)

  def apply(member: Membership, childSources: DataSource*): List[DataSource] = {
    val clientDS = member.clientId.obj.map(new MongoDataSource("association", _))
    logger.info("FIXME: Uses only first garden assigned to member")
    val gardenDS: Box[DataSource] = member.gardenRef.objs.headOption.map(new MongoDataSource("garden", _))
    List(new MongoDataSource("member", member, childSources: _*)) ::: List(clientDS, gardenDS).flatten
  }
}

//object GardenDataSource extends Loggable {
//
//  def apply(garden: Garden): DataSource = {
//    val members = Garden.findMembersOfGarden(garden)
//    new CompositeDataSource(garden, members: _*)
//  }
//}

//object MembershipDataSource extends Loggable {
//
//  def apply(recipient: Membership): List[DataSource] = {
//    List(new MongoDataSource("member", recipient))
//  }
//}

object DocumentDataSource extends Loggable {

  def apply(docHolder: GeneratedDocument): DataSource = apply(docHolder, Nil: _*)

  def apply(docHolder: GeneratedDocument, childSources: DataSource*): DataSource = {
    new MongoDataSource("document", docHolder, childSources: _*)
  }
}

object AdditionalMapDataSource extends Loggable {
  def apply(additional: Map[String, String]): DataSource = new MapDataSource("additional", additional)
}

object OutputDocumentInvoiceDataSource {

  def apply(invoice: InvoiceContainer, generatedInvoice: GeneratedInvoice): DataSource = {
    val filename = s"Rechnung ${generatedInvoice.invoiceNo.get} vom ${invoice.invoiceDate.asText}"
    val map = Map[String, String]("FileName" -> filename)
    new MapDataSource("OutputDocument", map)
  }

}
