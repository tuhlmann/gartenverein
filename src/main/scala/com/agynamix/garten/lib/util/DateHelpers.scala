package com.agynamix.garten.lib.util

import java.text.SimpleDateFormat
import java.util.Date
import net.liftweb.util.Helpers

object DateHelpers extends DateHelpers

trait DateHelpers {

  val germanShortDate = new SimpleDateFormat("dd.MM.yyyy")
  
}