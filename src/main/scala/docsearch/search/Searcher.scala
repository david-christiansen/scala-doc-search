//Used as an API for interacting with the search
package docsearch.search

import collection.mutable
import docsearch.query.{QueryParser, Query, QArg}
import docsearch.types.Member

class Searcher(state: SearchState[Query]) {
  def findResults(): List[Member] = state.peek.findMatching ++ findResults(0)
  //Right now just make 50 results.  This becomes lazy and infinite later.
  def findResults(n: Int): List[Member] = {
    println("finding results for " + state.peek)
    if (n > 50) Nil
    else {
      state.step match {
        case Some(q) => {
          println("got new query "+q)
          val newRes = q.findMatching.take(50 - n)
          newRes ++ findResults(n + newRes.length)
        }
        case None => Nil
      }
    }
  }
}

object Searcher {
  def search(query: String): Option[Searcher] = {
    try {
      val q = ParseQ.parseQ(query)
      val search = new Searcher(new SearchState(q, Edits.addOptionResult)(SearchNode.nodeOrdering))
       Some(search)
    } catch {
      case _ => None
    }
  }

  object ParseQ extends QueryParser {
    def parseQ(input: String) = {
      val q = parse(input, query)
      q match {
        case Success(p: Query, _) => p.asInstanceOf[Query]
        case _ => throw new Exception("Failed to parse")
      }
    }
  }
}
