package com.agynamix.garten.model.share

import net.liftweb.record.Record
import net.liftweb.util.Helpers._
import net.liftweb.common._
import com.foursquare.rogue.LiftRogue._
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.record.field.EnumNameField
import com.agynamix.garten.model.ScheduledJobState

  trait Schedulable[OwnerType <: Schedulable[OwnerType]] extends Record[OwnerType] {
    self: OwnerType =>

    object jobState      extends EnumNameField(this, ScheduledJobState) {
      override def defaultValue = ScheduledJobState.Open
    }

  }
