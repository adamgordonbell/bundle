import org.scalatest.{Matchers, FunSuite}

class Tests extends FunSuite with Matchers {
  val apple = Product(1,"Apple")
  val bread = Product(2,"Bread")
  val butter = Product(3,"Butter")

  val products: List[Product] = List(
    apple,
    bread,
    butter
  )
  val pricing = List(
    Price(apple, 1.99),
    Price(bread, 3.99),
    Price(butter, 2.50)
  )

  //Two Apples are 2.15
  val twoApples = Bundle(List(
      Price(apple,2.15),
      Price(apple,0)
    ))

  //Buy Bread and a stick of butter and the second stick is free
  val freeStickOfButter = Bundle(List(
      Price(bread, 3.99),
      Price(butter, 2.50),
      Price(butter, 0)
  ))

  val bundles = List(twoApples,freeStickOfButter)

  test("no bundles") {
    BundlePricing(pricing, List.empty).price(products) shouldEqual 8.48
  }

  test("two apples") {
    BundlePricing(pricing, bundles).price(List(apple, apple)) shouldEqual 2.15
  }
}
