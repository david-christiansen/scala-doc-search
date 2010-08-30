package websearch.comet

import scala.collection.immutable.Stream
import scala.xml._

import net.liftweb._
import net.liftweb.http._
import SHtml._ 
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds.{SetHtml, Noop}
import net.liftweb.http.js.JE.Str

import search._

class Search extends CometActor with CometListener {
  override def defaultPrefix = Full("search")

  def registerWith = SearchServer.getServer(term)

  private var term = ""
  private var from = 0
  private var count = 10

  private var results: List[String] = Nil

  private def renderResults = {
    val resHtml = results.drop(from).take(count).flatMap(r => <li>{r}</li>)
    <ul>{resHtml}</ul>
  }

  def render = bind("search", 
                    "search" -> ajaxText(term, doSearch _), 
                    "next" -> a(doNext _, Text("Next >>")),
                    "prev" -> (if (from <= 0) Text("<< Prev") else a(doPrev _, Text("<< Prev"))),
                    "count" -> ajaxSelectObj[Int](1 to 5 map (x => (x * 10) -> (x * 10).toString), Full(count), setCount _),
                    "showing" -> showing _,
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
    registerWith ! (from + count)
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


