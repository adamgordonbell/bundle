import data._
import scala.collection.immutable.Bag
import util.Extend._
/**
  *
  * Created by adam on 8/13/16.
  */

  case class PricingService(basePrices: Map[SKU, BigDecimal], discounts : List[Discount]){

    def price(products : Bag[SKU]) : BigDecimal = {
      val basePrice = basePriceSum(products)
      val price = applyDiscount(PartialPrice(products))
      price.fullPrice(basePrices)
    }

    def applyDiscount(price : PartialPrice) : PartialPrice = {
      getAllDiscountsThatApply(price.items) match {
        case list =>   list.map(x =>  applyDiscount(price.applyDiscount(x))).sortBy(_.fullPrice(basePrices)).head
        case Nil => price
      }
    }

    def getAllDiscountsThatApply(products : Bag[SKU]) : List[Discount] = {
      val d = discounts.filter(bundle => bundle.items.intersect(products) == bundle.items)
      d //ToDo remove me
    }

    def basePriceSum(p : Bag[SKU]): BigDecimal = p.map(basePrices(_)).sum
  }
