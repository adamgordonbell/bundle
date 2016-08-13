package util

import scala.collection.immutable.Bag

/**
  * Created by adam on 8/13/16.
  */
object Extend {
  type SKU  = Int
  implicit val m1 = Bag.configuration.compact[SKU]
}
