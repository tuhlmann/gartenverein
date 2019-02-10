package com.agynamix.garten.lib.util

import java.util.ResourceBundle
import java.util.Locale
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.ThreadGlobal
import net.liftweb.http.LiftRules
import net.liftweb.util.Props
import net.liftweb.util.NamedPF
import scala.xml.Node
import scala.xml.NodeSeq
import net.liftweb.http.S

object L extends L

trait L {

  private val _resBundle = new ThreadLocal[List[ResourceBundle]]()

  def locale: Locale = LiftRules.localeCalculator(S.request.map(_.request))

  /**
   * Get a List of the resource bundles for the current locale. The resource bundles are defined by
   * the LiftRules.resourceNames and LiftRules.resourceBundleFactories variables.
   * If you do not define an entry for a particular key, we fall back to using
   * Lift's core entries.
   *
   * @see LiftRules.resourceNames
   * @see LiftRules.resourceBundleFactories
   */
  def resourceBundles: List[ResourceBundle] = resourceBundles(locale)

  def resourceBundles(loc: Locale): List[ResourceBundle] = {
    Box.legacyNullTest(_resBundle.get) match {
      case Full(bundles) => println("ResourceBundles set"); bundles
      case _ => {
        println("Setting _resBundle")
        _resBundle.set(
          LiftRules.resourceForCurrentLoc.vend() :::
          LiftRules.resourceNames.flatMap(name => tryo{
              if (Props.devMode) {
                tryo{
                  val clz = this.getClass.getClassLoader.loadClass("java.util.ResourceBundle")
                  val meth = clz.getDeclaredMethods.
                  filter{m => m.getName == "clearCache" && m.getParameterTypes.length == 0}.
                  toList.head
                  meth.invoke(null)
                }
              }
          List(ResourceBundle.getBundle(name, loc))
        }.openOr(
          NamedPF.applyBox((name, loc), LiftRules.resourceBundleFactories.toList).map(List(_)) openOr Nil
          )))
        _resBundle.get
      }
    }
  }


  /**
   * Get a localized string or return the original string.
   * We first try your own bundle resources, if that fails, we try
   * Lift's core bundle.
   *
   * @param str the string to localize
   *
   * @return the localized version of the string
   *
   * @see # resourceBundles
   */
  def ?(str: String): String = {
    try {
      S ? str
    } catch {
      case e: Exception => println("Fallback to our localization");  ?!(str, resourceBundles)
    }
  }

  /**
   * Attempt to localize and then format the given string. This uses the String.format method
   * to format the localized string.
   * We first try your own bundle resources, if that fails, we try
   * Lift's core bundle.
   *
   * @param str the string to localize
   * @param params the var-arg parameters applied for string formatting
   *
   * @return the localized and formatted version of the string
   *
   * @see String.format
   * @see # resourceBundles
   */
  def ?(str: String, params: Any*): String = {
    if (params.length == 0) {
      ?(str)
    } else {
      String.format(locale, ?(str), params.flatMap {case s: AnyRef => List(s) case _ => Nil}.toArray: _*)
    }
  }


  private def ?!(str: String, resBundle: List[ResourceBundle]): String = resBundle.flatMap(r => tryo(r.getObject(str) match {
    case s: String => Full(s)
    case n: Node => Full(n.text)
    case ns: NodeSeq => Full(ns.text)
    case _ => Empty
  }).flatMap(s => s)).find(s => true) getOrElse {
    LiftRules.localizationLookupFailureNotice.foreach(_(str, locale));
    str
  }

}