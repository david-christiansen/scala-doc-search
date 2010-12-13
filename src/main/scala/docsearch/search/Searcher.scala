//Used as an API for interacting with the search
package docsearch.search

import _root_.net.liftweb.actor._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JE._
import _root_.net.liftweb.http.js.jquery.JqJE._
import _root_.net.liftweb.http.js.jquery.JqJsCmds._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._

import collection.mutable
import docsearch.query.{QueryParser, Query, QArg}
import docsearch.types.Member

class Searcher(state: SearchState[Query]) {
  def findResults(): List[Member] = findResults(0)
  //Right now just make 2000 results.  This becomes lazy and infinite later.
  def findResults(n: Int): List[Member] = {
    println("finding results for " + state.peek)
    if (n > 2000) Nil
    else {
      println("else branch")
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
      val search = new Searcher(new SearchState(q, Edits.addOptionResult, Edits.addOptionArg)(SearchNode.nodeOrdering))
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


case object GetMore

import docsearch.web.comet.Results

class CometSearcher(state: SearchState[Query]) extends LiftActor with ListenerManager {
  var resultList: List[(Query, List[Member])] = Nil

  protected def createUpdate = Results(resultList)

  override def lowPriority = {
    case GetMore => state.step match {
      case Some(newQ) => {resultList ++= List((newQ, newQ.findMatching())); updateListeners(); this ! GetMore}
      case None => ()
    }
  }

  this ! GetMore
}

object CometSearcher {
  private var registry: mutable.Map[String, Option[CometSearcher]] = mutable.Map.empty

  def search(query: String): Option[CometSearcher] = {
    if (registry contains query) registry(query)
    else {
      val searcher = try {
        val q = ParseQ.parseQ(query)
        val search = new CometSearcher(new SearchState(q, Edits.defaultEdits:_*)(SearchNode.nodeOrdering))
        Some(search)
      } catch {
        case _ => None
      }
      registry += query -> searcher
      searcher
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
