package websearch.comet

import scala.collection.immutable.Stream

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
    val resHtml = results flatMap (r => <li>{r}</li>)
    <ul>{resHtml}</ul>
  }

  def render = bind("search", "search" -> ajaxText(term, doSearch _), "results" -> renderResults)

  override def lowPriority : PartialFunction[Any, Unit] = {
    case (newRes: List[String], done) => {
      results = newRes
      reRender(false)
    }
    case msg => {
      S.notice("bar")
      results = List("failure", msg toString)
      reRender(false)
    }
  }

  private def doSearch(newTerm: String) = {
    registerWith ! RemoveAListener(this)
    term = newTerm
    registerWith ! AddAListener(this, shouldUpdate)
    reRender(false)
    Noop
  }
}


