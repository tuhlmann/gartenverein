package com.agynamix.garten.lib.plan

/**
 * @param name the name of the plan
 * @param maxNumberOfClients the max number of clients that the user can create with this plan
 * @param costPerMonth the monthly cost in cent
 */
abstract sealed class PaymentPlanType(val name: String, maxNumberOfCients: Int, costPerMonth: Int)

case object Free extends PaymentPlanType("free", 99, 0)

object PaymentPlan {

  def apply(name: String): PaymentPlanType = name match {
    case Free.name => Free
  }

}
