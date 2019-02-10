package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.mongodb.record.field.ObjectIdPk
import com.agynamix.garten.model.share.CreatedUpdated
import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.record.field.EmailField
import com.agynamix.garten.lib.field.BsEmailField

object NewsletterSubscriber extends NewsletterSubscriber with MongoMetaRecord[NewsletterSubscriber] {

  def add(email: String) = NewsletterSubscriber.createRecord.email(email)

}

class NewsletterSubscriber private() extends MongoRecord[NewsletterSubscriber] with ObjectIdPk[NewsletterSubscriber] with CreatedUpdated[NewsletterSubscriber] {
  def meta = NewsletterSubscriber

  object email extends BsEmailField(this, 200)

}