import scala.collection.immutable.Bag
/**
  *
  * Created by adam on 8/13/16.
  */
case class Product(sku : Int, name : String)
case class Price(product : Product, price : BigDecimal)
case class Bundle(prices : List[Price])

case class BundlePricing(basePricing : List[Price], bundles : List[Bundle])
{
  //we are going to use bags (multi-sets) of SKUs (the int id of our products) for our internal representation
  type SKU  = Int
  implicit val m1 = Bag.configuration.compact[SKU]
  case class Discount(items : Bag[SKU], basePrice :BigDecimal, discountPrice : BigDecimal){
    val savings = discountPrice - basePrice
  }

 val basePriceMap: Map[SKU, BigDecimal] = basePricing.map(x => (x.product.sku,x.price)).toMap
 val discounts = getBundleDiscounts(basePricing, bundles)
  def price(products : List[Product]) : BigDecimal = {
    val productsBag: Bag[SKU] = Bag(products.map(_.sku) : _*)
    val basePrice = basePriceSum(productsBag)

    val (items, price) = discount(productsBag)
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
    d
  }

  def getAllDiscountsThatApply(products : Bag[SKU]) : List[Discount] = {
    val d = discounts.filter(bundle => bundle.items.intersect(products) == bundle.items)
    d
  }


  def getBundleDiscounts(basePricing : List[Price],bundles : List[Bundle]): List[Discount] =
  {
    def discount(b : Bundle) : Discount = {
      val baseCost = b.prices.map(p => basePriceMap(p.product.sku)).sum
      val discountCost = b.prices.map(_.price).sum
      val bag =  Bag(b.prices.map(_.product.sku) : _*)
      Discount(bag, baseCost, discountCost)
    }
   bundles.map(discount(_))
  }

  def basePriceSum(p : Bag[SKU]): BigDecimal = p.map(basePriceMap(_)).sum
}
