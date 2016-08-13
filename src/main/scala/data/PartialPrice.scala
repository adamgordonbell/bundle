package data

import util.Extend.SKU

import scala.collection.immutable.Bag

/**
  * Created by adam on 8/13/16.
  */
case class PartialPrice(items : Bag[SKU], price : BigDecimal = 0){
  def fullPrice(basePrices : Map[SKU,BigDecimal])= {
    items.map(basePrices(_)).sum + price
  }

  def applyDiscount(discount : Discount): PartialPrice = {
    PartialPrice(this.items.diff(discount.items), this.price + discount.discountPrice)
  }
}
