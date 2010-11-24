package docsearch.search

import org.scalacheck._

object SearchNodeOrderingSpec extends Properties("SearchNodeOrdering") {
  val ordering = implicitly[Ordering[SearchNode[Double]]]

  property("compare") = Prop.forAll {(a: Double, b: Double) => 
    val result = ordering.compare(SearchNode(a, a), SearchNode(b, b))
    if (a < b) result < 0
    else if (a > b) result > 0
    else result == 0
  }

  property("equal") = Prop.forAll(
    (a: Double) => 
      ordering.compare(SearchNode(a, a), SearchNode(a, a)) == 0
  )
}

