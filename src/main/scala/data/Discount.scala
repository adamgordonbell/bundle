package data

import scala.collection.immutable.Bag

/**
  * Created by adam on 8/13/16.
  */
case class Discount(items: Bag[Int], basePrice: BigDecimal, discountPrice: BigDecimal) {
  val savings = discountPrice - basePrice
}
