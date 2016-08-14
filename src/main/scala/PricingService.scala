import data._
import util.Extend._

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Bag

/**
  *
  * Created by adam on 8/13/16.
  */

case class PricingService(basePrices: Map[SKU, BigDecimal], discounts: List[Discount]) {

  //Cache sub-problems using concurrent threadsafe cache
  val cache = new TrieMap[String, BigDecimal]

  def price(products: Bag[SKU]): BigDecimal = {
    applyDiscount(products)
  }

  def applyDiscount(remainingItems: Bag[SKU]): BigDecimal = {
    def getCombinationWithLowestPrice(list: List[Discount]): BigDecimal = {
      def subtractDiscount(items: Bag[SKU], discount: Discount): Bag[SKU] = {
        items.diff(discount.items)
      }
      val stepOneResult = list.map(x => (subtractDiscount(remainingItems, x), x))
      val stepNResult = stepOneResult.map(x => x._2.discountPrice + applyDiscount(x._1))
      stepNResult.sortWith(_ < _).head
    }
    def apply: BigDecimal = {
      def basePriceSum(products: Bag[SKU]): BigDecimal = {
        def default() : BigDecimal = 0
        products.map(basePrices.getOrElse(_,default)).sum
      }
      def getAllDiscountsThatApply(products: Bag[SKU]): List[Discount] = {
        discounts.filter(bundle => bundle.items.intersect(products) == bundle.items)
      }
      getAllDiscountsThatApply(remainingItems) match {
        case Nil => basePriceSum(remainingItems)
        case list => getCombinationWithLowestPrice(list)
      }
    }
    cache.getOrElseUpdate(remainingItems.toString, apply)
  }
}
