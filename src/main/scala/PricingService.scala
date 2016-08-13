import data._
import scala.collection.immutable.Bag
import util.Extend._
/**
  *
  * Created by adam on 8/13/16.
  */

  case class PricingService(basePrices: Map[SKU, BigDecimal], discounts : List[Discount]){

    def price(products : Bag[SKU]) : BigDecimal = {
      applyDiscount(products)
    }


    def applyDiscount(remainingItems : Bag[SKU]) : BigDecimal = {
      println("applyDiscount")
      println(remainingItems)
      def applyAllDiscounts(list: List[Discount]): BigDecimal = {
        val l = list.map(x => (subtractDiscount(remainingItems,x),x))
        val x = l.map(x => x._2.discountPrice + applyDiscount(x._1))
        x.sortWith(_ < _).head
      }
      getAllDiscountsThatApply(remainingItems) match {
        case Nil => basePriceSum(remainingItems)
        case list => val l = applyAllDiscounts(list)
          l
      }
    }

    def getAllDiscountsThatApply(products : Bag[SKU]) : List[Discount] = {
      val d = discounts.filter(bundle => bundle.items.intersect(products) == bundle.items)
      d //ToDo remove me
    }

    def basePriceSum(p : Bag[SKU]): BigDecimal = p.map(basePrices(_)).sum

   def subtractDiscount(items: Bag[SKU], discount : Discount): Bag[SKU] = {
     items.diff(discount.items)
  }

  }
