import data.{Bundle, Price, Product}
import org.scalatest.{FunSuite, Matchers}

class Tests extends FunSuite with Matchers {
  val apple = Product(1, "Apple")
  val bread = Product(2, "Bread")
  val butter = Product(3, "Butter")

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
    Price(apple, 2.15),
    Price(apple, 0)
  ))

  //Buy Bread and a stick of butter and the second stick is free
  val freeStickOfButter = Bundle(List(
    Price(bread, 3.99),
    Price(butter, 2.50),
    Price(butter, 0)
  ))

  val bundles = List(twoApples, freeStickOfButter)

  test("no bundles") {
    PricingAPI(pricing, List.empty).price(products) shouldEqual 8.48
  }

  test("two apples") {
    PricingAPI(pricing, bundles).price(List(apple, apple)) shouldEqual 2.15
  }


  test("greedy algo fail") {
    val (a, b, c, d) = (
      Product(1, "A"),
      Product(2, "B"),
      Product(3, "C"),
      Product(4, "D")
      )
    val products: List[Product] = List(
      a,
      b,
      c,
      d
    )
    val pricing = products.map(Price(_, 10))

    val greedyDiscount = Bundle(List(
      Price(a, 10),
      Price(b, 10),
      Price(c, 10),
      Price(d, 0)
    ))

    val non_greedy_A = Bundle(List(
      Price(a, 13),
      Price(b, 0)
    ))

    val non_greedy_B = Bundle(List(
      Price(c, 12),
      Price(d, 0)
    ))

    PricingAPI(pricing, List(greedyDiscount, non_greedy_A, non_greedy_B)).price(List(a, b, c, d)) shouldEqual 25
  }


  test("Combinatorial Discount - Cache Test") {
    val (a, b, c, d) = (
      Product(1, "A"),
      Product(2, "B"),
      Product(3, "C"),
      Product(4, "D")
      )
    val products: List[Product] = List(
      a,
      b,
      c,
      d
    )
    val pricing = products.map(Price(_, 10))

    val greedyDiscount = Bundle(List(
      Price(a, 10),
      Price(b, 10),
      Price(c, 10),
      Price(d, 0)
    ))

    val nongreedyA = Bundle(List(
      Price(a, 13),
      Price(b, 0)
    ))

    val nongreedyB = Bundle(List(
      Price(c, 12),
      Price(d, 0)
    ))

    val bad1 = Bundle(List(
      Price(a, 15),
      Price(b, 0)
    ))


    val bad1a = Bundle(List(
      Price(a, 16),
      Price(b, 0)
    ))

    val bad2 = Bundle(List(
      Price(a, 17),
      Price(b, 0)
    ))

    val bad3 = Bundle(List(
      Price(a, 18),
      Price(b, 0)
    ))

    val bad4 = Bundle(List(
      Price(a, 19),
      Price(b, 0)
    ))

    val bad5 = Bundle(List(
      Price(c, 122),
      Price(d, 0)
    ))

    PricingAPI(pricing, List(greedyDiscount, nongreedyA, nongreedyB, bad1, bad1a, bad2, bad3, bad4, bad5)).price(List(a, b, c, d)) shouldEqual 25
  }
}
