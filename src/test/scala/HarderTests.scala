import data.{Bundle, Price, Product}
import org.scalatest.{FunSuite, Matchers}

class HarderTests extends FunSuite with Matchers {
  val (a1, b2, c3, d4) = (
    Product(1, "A"),
    Product(2, "B"),
    Product(3, "C"),
    Product(4, "D")
    )
  val products: List[Product] = List(
    a1,
    b2,
    c3,
    d4
  )
  val pricing = products.map(Price(_, 10))

  val greedyDiscount = Bundle(List(
    Price(a1, 10),
    Price(b2, 10),
    Price(c3, 10),
    Price(d4, 0)
  ))

  val non_greedy_A = Bundle(List(
    Price(a1, 13),
    Price(b2, 0)
  ))

  val non_greedy_B = Bundle(List(
    Price(c3, 12),
    Price(d4, 0)
  ))

  test("greedy search fail") {
    PricingAPI(pricing, List(greedyDiscount, non_greedy_A, non_greedy_B)).price(List(a1, b2, c3, d4)) shouldEqual 25
  }

  val junkDiscounts = List.fill(20)(Bundle(List(
    Price(a1, 15),
    Price(b2, 0)
  )))

  test("combinatorial discount - Cache Test"){
    PricingAPI(pricing, List(greedyDiscount, non_greedy_A, non_greedy_B) ++ junkDiscounts).price(List(a1, b2, c3, d4)) shouldEqual 25
    PricingAPI(pricing, List(greedyDiscount, non_greedy_A, non_greedy_B) ++ junkDiscounts).price(List(a1, b2, c3, d4)) shouldEqual 25
  }
}
