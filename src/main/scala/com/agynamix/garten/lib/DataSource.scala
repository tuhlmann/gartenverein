package com.agynamix.garten.lib

import net.liftweb.common._
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.util.BaseField
import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.common.Box.box2Option
import net.liftweb.common.Box.option2Box
import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.mongodb.record.field.ObjectIdPk
import com.agynamix.document.MarkInvoicedData

object DataSourcePropertyType extends Enumeration {
  type DataSourcePropertyType = Value

  val Text  = Value("text")
  val Image = Value("image")

}

case class DataSourceProperty(id: String, value: Box[String] = Empty, propType: DataSourcePropertyType.Value = DataSourcePropertyType.Text)

trait DataSource {

  def datasourceModule: String
  def children: List[DataSource] = Nil

  def findModuleByName(modules: List[String]): Box[DataSource] = modules match {
    case Nil => Full(this)
    case module :: lst => getModule(module).flatMap(m => m.findModuleByName(lst))
  }

  def getModule(name: String): Box[DataSource] = {
    if (this.datasourceModule == name) Full(this) else children.find(c => c.datasourceModule == name)
  }

  def baseFieldToString(f: BaseField): String = {
    f.get match {
      case s: String => f.get.toString
      case _ => f.asHtml.toString()
    }
  }

  /**
   * Get a DataSource property value as a string,
   * used for displaying in a document or on screen
   */
  def getDisplayProperty(key: String): Box[String] = {
    getRawProperty(key) match {
      case Full(f: BaseField) => Full(baseFieldToString(f))
      case Full(value)        => Full(value.toString())
      case _                  => Empty
    }
  }

  /**
   * Get a typed property value, useful for further computation
   */
  def getTypedProperty(key: String): Box[Any] = {
    getRawProperty(key) match {
      case Full(f: BaseField) => Full(f.get)
      case other => other
    }
  }

  /**
   * Get the property value with the unmodified data type.
   * Returns a BaseField for record values or a typed value otherwise
   */
  def getRawProperty(key: String): Box[Any]

  /**
   * Clients can override that to add there own properties
   */
  def clientGetRawProperty: PartialFunction[String, Any] = Map.empty

  /**
   * Certain DataSources can offer repeater elements
   */
  def getRepeatedElems(key: String): List[DataSource] = Nil

  /**
   * A feedback that the value corresponding to the given key has been invoiced.
   * The DataDource may or may not want to log that so that this values will not be processed again.
   */
  def markValueInvoiced(key: String, markInvoicedData: MarkInvoicedData): Unit = {}


}

object EmptyDataSource extends DataSource {
  val datasourceModule = "empty"

  def getRawProperty(key: String): Box[Any] = Empty
}

trait MongoDataSourceTrait[BaseRecord <: MongoIdRecord[BaseRecord] with ObjectIdPk[BaseRecord]]
      extends MongoIdRecord[BaseRecord] with DataSource {

  self: BaseRecord =>

  def getRawProperty(key: String): Box[Any] = clientGetRawProperty.lift(key) orElse {
    allRecordFields.find(_.name == key)
  }
}

class MongoDataSource(val datasourceModule: String, val data: MongoIdRecord[_], childDS: DataSource*) extends DataSource {

  override val children = childDS.toList

  def getRawProperty(key: String): Box[Any] = clientGetRawProperty.lift(key) orElse {
    data.allRecordFields.find(_.name == key)
  }

}

trait BsonRecordDataSourceTrait[BaseRecord <: BsonRecord[BaseRecord]] extends BsonRecord[BaseRecord] with DataSource {

  self: BaseRecord =>

  def getRawProperty(key: String): Box[Any] = clientGetRawProperty.lift(key) orElse {
    fieldByName(key)
  }

}

class BsonRecordDataSource(val datasourceModule: String, val data: BsonRecord[_], childDS: DataSource*) extends DataSource {
  override val children = childDS.toList

  def getRawProperty(key: String): Box[Any] = clientGetRawProperty.lift(key) orElse {
    val fieldBox: Box[BaseField] = data.fieldByName(key)
    fieldBox
  }

}

class CompositeDataSource(val data: DataSource, childDS: DataSource*) extends DataSource {

  override val children = childDS.toList
  val datasourceModule = data.datasourceModule

  def getRawProperty(key: String): Box[Any] = data.getRawProperty(key)
  override def getRepeatedElems(key: String): List[DataSource] = data.getRepeatedElems(key)
}


class MapDataSource(val datasourceModule: String, val data: Map[String, String], childDS: DataSource*) extends DataSource {
  override val children = childDS.toList
  def getRawProperty(key: String): Box[Any] = data.get(key)
}