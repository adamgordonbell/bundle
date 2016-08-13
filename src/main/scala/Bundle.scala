/**
  * Created by adam on 8/13/16.
  */
case class Product(sku : Int, name : String)
case class Price(product : Product, price : BigDecimal)
case class Bundle(products : List[Product], price : BigDecimal)


case class BundlePricing(pricing : List[Price], bundles : List[Bundle])
{
  def price(products : List[Product]) : BigDecimal = {
   0
  }
}
