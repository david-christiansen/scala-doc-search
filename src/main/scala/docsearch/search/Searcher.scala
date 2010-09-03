package docsearch.search

import collection.mutable.Buffer

import net.liftweb.actor._
import net.liftweb.http._

abstract sealed class SearcherRequest
case class Expand(num: Int) extends SearcherRequest
case object Go extends SearcherRequest


abstract sealed class SearcherReply
case object DoneSearching extends SearcherReply
case class NextResult(res: String) extends SearcherReply

class Searcher(server: SearchServer, term: String) extends SpecializedLiftActor[SearcherRequest] {
  var count = 0
  var searchTo = 0
  
  def messageHandler = {
    case Expand(num) => {
      if (num > searchTo) searchTo = num
    }
    case Go => {
      if (count < searchTo) {
        server ! NextResult("Result " + count + " for '" + term + "'")
        count += 1
        Thread.sleep(500L)
        this ! Go
      }
      else {
        server ! DoneSearching
      }
    }
  }
  def go() = this ! Go
}

class SearchServer(term: String) extends LiftActor with ListenerManager {
  var results: Buffer[String] = Buffer.empty
  var finalResults: List[String] = List()
  var done = false
  val searcher = new Searcher(this, term)

  def createUpdate = (if (done) finalResults else results toList, done)

  override def lowPriority = {
    case DoneSearching => {
      done = true
      finalResults = results.toList
      updateListeners()
    }
    case NextResult(res) => {
      results += res
      updateListeners()
    }
    case a: Int => {
      searcher ! Expand(a)
      done = false
      searcher go
    }
  }

  searcher ! Expand(20)
  searcher go
}

object SearchServer {
  private var registry: Map[String, SearchServer] = Map.empty
  def getServer(term: String) = {
    registry.get(term) match {
      case Some(server) => server
      case None => {
        val server = new SearchServer(term)
        registry += (term -> server)
        server
      }
    }
  }
}


