package docsearch.search

import scala.collection.mutable.{PriorityQueue, Set}

/**
 * An item on the fringe of the graph
 */
sealed case class SearchNode[A](item: A, cost: Double)

object SearchNode {
  /**
   * Order search nodes by their associated costs.
   */
  implicit def nodeOrdering[A]: Ordering[SearchNode[A]] = 
   new Ordering[SearchNode[A]] {
     override def compare(n1: SearchNode[A], n2: SearchNode[A]): Int =
       if (n1.cost == n2.cost) 0
       else if (n1.cost < n2.cost) -1
       else 1
   }
}

class SearchState[A](start: A, neighborFinders: (A => Traversable[(A, Double)])*)(implicit ord: Ordering[SearchNode[A]]) {
  val fringe: PriorityQueue[SearchNode[A]] = new PriorityQueue()
  val seen: Set[A] = Set.empty

  fringe.enqueue(SearchNode(start, 0))
  seen += start

  def hasMore(): Boolean = fringe.size < 1

  def step(): A = {
    val next = fringe.dequeue
    seen += next.item
    for ((neighbor, cost) <- getNeighbors(next.item)) {
      assert(cost >= 0)
      if (!seen.contains(neighbor))
        fringe.enqueue(SearchNode(neighbor, cost + next.cost))
    }
    next.item
  }

  def getNeighbors(item: A): Traversable[(A, Double)] =
    neighborFinders flatMap (_.apply(item))

  lazy val results = Stream.continually(step)
}
