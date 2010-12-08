package docsearch.web.comet

import scala.collection.immutable.Stream
import scala.xml._

import net.liftweb._
import net.liftweb.http._
import SHtml._ 
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import net.liftweb.util.HttpHelpers._
import net.liftweb.http.js.JsCmds.{SetHtml, Noop}
import net.liftweb.http.js.JE.Str

import docsearch.search._
import docsearch.query._

class SearchComet extends CometActor with CometListener {
  override def defaultPrefix = Full("search")

  def registerWith = SearchServer.getServer(term)

  private var term = S.param("q") openOr ""
  private var from = 0
  private var count = 10

  private var results: List[String] = Nil


  private def renderResults = {
    Search.search(term) match {
      case Some(res) => { 
        val resHtml = res.results.drop(from).take(count).flatMap(r => <li>{r}</li>)
        <ul>{resHtml}</ul>
      }
      case None => <div>Could not parse <pre>{term}</pre></div>
    }
  }
  
  def render = bind("search", 
                    "search" -> ajaxText(term, doSearch _),
                    "next" -> a(doNext _, Text("Next >>")),
                    "prev" -> (if (from <= 0) Text("<< Prev") else a(doPrev _, Text("<< Prev"))),
                    "count" -> ajaxSelectObj[Int](1 to 5 map (x => (x * 10) -> (x * 10).toString), Full(count), setCount _),
                    "showing" -> showing _,
                    "permalink" -> <a>Permalink</a> % ("href" -> ("?q="+urlEncode(term))),
                    "results" -> renderResults)

  override def lowPriority : PartialFunction[Any, Unit] = {
    case (newRes: List[String], done) => {
      results = newRes
      reRender(false)
    }
    case msg => {
      results = List("failure", msg toString)
      reRender(false)
    }
  }

  private def doSearch(newTerm: String) = {
    count = 10
    registerWith ! RemoveAListener(this)
    term = newTerm
    registerWith ! AddAListener(this, shouldUpdate)
    reRender(false)
    Noop
  }

  private def update() = {
    registerWith ! (from + 2*count)
    reRender(false)
  }

  private def doNext() = {
    from += count
    update()
    Noop
  }

  private def doPrev() = {
    from = math.max(from - count, 0)
    update()
    Noop
  }
  private def setCount(newCount: Int) = {
    count = newCount
    update()
    Noop
  }

  private def showing(src: NodeSeq) = 
    bind(
      "showing", src, 
      "start" -> Text(from toString), 
      "end" -> Text((from + count).toString)
    )
}


