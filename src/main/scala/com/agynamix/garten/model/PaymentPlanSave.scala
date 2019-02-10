package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.share.UserId

//object PaymentPlanType extends Enumeration {
//  type PaymentPlanType = Value
//
//  val Free = Value("free")
//
//  def getString(value: Value) = value match {
//    case Free => "Free Plan"
//  }
//}
//
//
//object PaymentPlan extends PaymentPlan with MongoMetaRecord[PaymentPlan] {
//
//
//
//  def checkFreePlanAvailable(): Unit = {
//    if ((PaymentPlan where (_.name eqs PaymentPlanType.Free.toString()) count) == 0) {
//      PaymentPlan.createRecord.name(PaymentPlanType)
//    }
//  }
//
//  def checkAllPlansAvailable(): Unit = {
//    checkFreePlanAvailable()
//  }
//
//}
//
///**
// * A PaymentPlan. Saves a definition for the amount a user pays as well as the number of clients
// */
//class PaymentPlan private() extends MongoRecord[PaymentPlan] with ObjectIdPk[PaymentPlan] with CreatedUpdated[PaymentPlan] {
//  def meta = PaymentPlan
//
//  /**
//   * Name of the payment plan
//   */
//  object name extends StringField(this, 100)
//
//  //plan type (enum) to know about payment and plan implicits. Plan type should define the number of allowed clients
//
//  /**
//   * Defines the maximum number of clients this payment plan supports. Defaults to 99
//   */
//  object maxNumberOfClients extends IntField(this, 99)
//
//}