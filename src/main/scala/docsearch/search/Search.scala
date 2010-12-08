//Used as an API for interacting with the search
package docsearch.search

import docsearch.query.{QueryParser, Query, QType, QArg}

object Search {
  def search(query: Query): SearchState[Query] = {  
    val search = new SearchState(query, Edits.addOptionResult, Edits.addOptionArg)   
    search  
  }
  
  def search(query: String): Option[SearchState[Query]] = {
    try {
      val q = ParseQ.parseQ(query)
      val search = new SearchState(q, Edits.addOptionResult, Edits.addOptionArg)   
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
