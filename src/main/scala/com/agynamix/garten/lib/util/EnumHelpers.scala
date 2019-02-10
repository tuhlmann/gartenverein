package com.agynamix.garten.lib.util

import net.liftweb.http.S
import scala.xml.Text
import net.liftweb.common._
import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml

trait LocalizedEnum {

  def locKeyBase: String

  def locKey[E <: Enumeration#Value](value: E) = locKeyBase+"."+value.toString()
  def localized[E <: Enumeration#Value](value: E) = L ? locKey(value)

  def buildDisplayList[E <: Enumeration#Value](values: List[E]): List[(Box[E], String)] = {
    values.map(a => (Full(a), localized(a)))
  }

  def asHtml[E <: Enumeration#Value](value: E) = Text(localized(value))

}

