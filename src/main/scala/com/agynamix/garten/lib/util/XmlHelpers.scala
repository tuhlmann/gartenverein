package com.agynamix.garten.lib.util

import scala.xml._
import scala.Option.option2Iterable

object XmlHelpers extends XmlHelpers

trait XmlHelpers {

  case class GenAttr(pre: Option[String], key: String, value: Seq[Node], next: MetaData) {
    def toMetaData = Attribute(pre, key, value, next)
  }

  def decomposeMetaData(m: MetaData): Option[GenAttr] = m match {
    case Null => None
    case PrefixedAttribute(pre, key, value, next) =>
      Some(GenAttr(Some(pre), key, value, next))
    case UnprefixedAttribute(key, value, next) =>
      Some(GenAttr(None, key, value, next))
  }

  def unchainMetaData(m: MetaData): Iterable[GenAttr] = m flatMap (decomposeMetaData)

  def chainMetaData(l: Iterable[GenAttr]): MetaData = l match {
    case Nil => Null
    case head :: tail => head.copy(next = chainMetaData(tail)).toMetaData
  }

  def mapMetaData(m: MetaData)(f: GenAttr => GenAttr): MetaData = chainMetaData(unchainMetaData(m).map(f))

  def attributesContainKey(m: MetaData, key: String): Boolean = unchainMetaData(m).exists(_.key == key)

  def attributesContainKeyValue(m: MetaData, key: String, value: String): Boolean = {
    val md = unchainMetaData(m)
//    val re = md.find(_.key == key).map(a => a.value.map(_.text).exists(_ == value)) getOrElse(false)
    val re = md.find(_.key == key).map(a => a.value.map(_.text)) getOrElse Nil
    val re2 = re.exists(v => v.split(" ").toList.exists(s => s.trim() == value))
    //println("Check for %s in %s: %s".format(value, re, re2))
    re2
  }

  def mergeAttributes(e: Elem, attr: MetaData): Elem = {
    val attrList = unchainMetaData(attr)
    val eList = unchainMetaData(e.attributes)
    var result = e
    for (a <- attrList) {
      if (attributesContainKey(e.attributes, a.key)) {
        result = e.copy(attributes = mapMetaData(e.attributes) {
                case g @ GenAttr(_, a.key, Text(v), _) => g.copy(value = Text(v + " " + a.value))
                case other => other
              })
      } else {
        result = e % (a.toMetaData)
      }
    }
    //println("Merge Result: "+result)
    result
  }


}