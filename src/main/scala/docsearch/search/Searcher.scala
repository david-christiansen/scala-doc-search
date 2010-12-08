//Used as an API for interacting with the search
package docsearch.search

import collection.mutable
import docsearch.query.{QueryParser, Query, QArg}
import docsearch.types.Member

class Searcher(state: SearchState[Query]) {
  //Right now just make 50 results.  This becomes lazy and infinite later.
  def findResults(n: Int = 0): List[Member] = {
    if (n > 50) Nil
    else {
      state.step match {
        case Some(q) => {
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
      val search = new Searcher(new SearchState(q, Edits.addOptionResult, Edits.addOptionArg))
      println("foo")
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
