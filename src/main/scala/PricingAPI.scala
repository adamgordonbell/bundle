import data._
import scala.collection.immutable.Bag
import util.Extend._
/**
  *
  * Created by adam on 8/13/16.
  */

//This is our API end-point
//Externally we use Products and Discounts for ease of calling
//Internally we are going to use Bags of SKUs and Discounts as they are easier to work with
//Here we do the conversion to the internal type

case class PricingAPI(basePricing : List[Price], bundles : List[Bundle])
{
  private val basePriceMap =  basePricing.map(x => (x.product.sku,x.price)).toMap
  private val service = PricingService(
    basePriceMap,
    bundles.map(_.toDiscount(basePriceMap))
  )

  def price(products : List[Product]) : BigDecimal = {
    val productsBag: Bag[SKU] = Bag(products.map(_.sku) : _*)
   service.price(productsBag)
  }

}

