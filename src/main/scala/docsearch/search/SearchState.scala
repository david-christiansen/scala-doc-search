package docsearch.search
import docsearch.query.{QueryParser, Query}

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
       else if (n1.cost < n2.cost) 1
       else -1
   }
}

class SearchState[A](start: A, neighborFinders: (A => Traversable[(A, Double)])*)(implicit ord: Ordering[SearchNode[A]]) {
  val fringe: PriorityQueue[SearchNode[A]] = new PriorityQueue()
  val seen: Set[A] = Set.empty
  private[this] var last: A = start

  fringe.enqueue(SearchNode(start, 0))
  seen += start

  def hasMore(): Boolean = fringe.size > 0

  def peek = last

  def step(): Option[A] = {
    if (!hasMore) None
    else {
      val next = fringe.dequeue
      for ((neighbor, cost) <- getNeighbors(next.item)) {
        assert(cost >= 0)
        if (!seen.contains(neighbor)) {
          seen += neighbor
          fringe.enqueue(SearchNode(neighbor, next.cost + cost))
        }
      }
      last = next.item
      Some(next.item)
    }
  }

  def getNeighbors(item: A): Traversable[(A, Double)] =
    neighborFinders flatMap (_.apply(item))

  lazy val results = Stream.continually(step)
}

