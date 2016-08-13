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
      val (items, price) = discount(products)
      basePriceSum(items) + price
    }

    def discount(products : Bag[SKU], price : BigDecimal = 0) : (Bag[SKU], BigDecimal) = {
      biggestDiscount(products) match {
        case Some(d) => discount(products.diff(d.items), price + d.discountPrice)
        case None =>  (products,price)
      }
    }
    def biggestDiscount(products : Bag[SKU]) : Option[Discount] = {
      val d = getAllDiscountsThatApply(products).sortBy(_.savings).headOption
      d //ToDo remove me
    }

    def getAllDiscountsThatApply(products : Bag[SKU]) : List[Discount] = {
      val d = discounts.filter(bundle => bundle.items.intersect(products) == bundle.items)
      d //ToDo remove me
    }

    def basePriceSum(p : Bag[SKU]): BigDecimal = p.map(basePrices(_)).sum
  }
