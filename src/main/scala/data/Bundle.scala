package data

import util.Extend._

import scala.collection.immutable.Bag

/**
  * Created by adam on 8/13/16.
  */

case class Bundle(prices: List[Price]) {
  def toDiscount(basePriceMap: Map[SKU, BigDecimal]) = {
    val baseCost = this.prices.map(p => basePriceMap(p.product.sku)).sum
    val discountCost = this.prices.map(_.price).sum
    val bag = Bag(this.prices.map(_.product.sku): _*)
    Discount(bag, baseCost, discountCost)
  }
}
