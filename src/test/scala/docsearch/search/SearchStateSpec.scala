package docsearch.search

import org.scalatest._

object SearchStateSpec extends FunSuite {
  val rand = new scala.util.Random // give seed if test should be repeatable

  test("increasing double search gives increasing doubles") {
    val getNeighbor = (a: Double) => {
      val howMany = rand.nextInt(5) + 1
      for (i <- 0 to howMany; val extra = rand.nextDouble)
        yield (a + extra, extra)
    }

    val search = new SearchState(0.0, getNeighbor)
    
    var last = -1.0
    for (res <- search.results.take(1000)) {
      assert(last < res)
      last = res
    }
  }
}
