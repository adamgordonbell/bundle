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
    def apply: BigDecimal = {
      getAllDiscountsThatApply(remainingItems) match {
        case Nil => basePriceSum(remainingItems)
        case list => getCombinationWithLowestPrice(remainingItems, list)
      }
    }
    cache.getOrElseUpdate(remainingItems.toString, apply)
  }

  private def getAllDiscountsThatApply(products: Bag[SKU]): List[Discount] = {
    def IsSubsetOfProducts(discount : Discount) : Boolean = {
      //println(products.count(_ => true) + discount.items.count(_ => true))
      discount.items.intersect(products) == discount.items
    }
    discounts.filter(IsSubsetOfProducts(_))
  }

  private def basePriceSum(products: Bag[SKU]): BigDecimal = {
    def default() : BigDecimal = 0
    products.map(basePrices.getOrElse(_,default)).sum
  }

  //Check all combinations starting with each discount via recursion and return the lowest
  private def getCombinationWithLowestPrice(remainingItems : Bag[SKU], list: List[Discount]): BigDecimal = {
    def subtractDiscount(items: Bag[SKU], discount: Discount): Bag[SKU] = {
      items.diff(discount.items)
    }
    val stepOneResult: List[(Bag[SKU], Discount)] = {
      list.map(x => (subtractDiscount(remainingItems, x), x))
    }
    val stepNResult: List[BigDecimal] = {
      stepOneResult.map(x => x._2.discountPrice + applyDiscount(x._1))
    }
    stepNResult.sortWith(_ < _).head
  }
}
