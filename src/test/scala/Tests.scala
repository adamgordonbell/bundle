import org.scalatest.{Matchers, FunSuite}

class Tests extends FunSuite with Matchers {
  val products: List[Product] = List(
    Product(1,"Apple"),
    Product(2,"Orange"),
    Product(3, "Bread")
  )
  val pricing: List[Price] = products.map(Price(_,1))

  val bundles = List(
    Bundle(products, 20),
    Bundle(products.take(2),20)
  )

  test("no bundles") {
    BundlePricing(pricing, List.empty).price(products) shouldEqual 3
  }
}
