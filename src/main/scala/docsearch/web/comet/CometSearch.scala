package docsearch.web
package comet

import scala.xml.{ Text, Node, NodeSeq }


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

import docsearch.types
import docsearch.query
import docsearch.utils._
import docsearch.search._

class FailSearcher extends LiftActor with ListenerManager {
  def createUpdate = Results(List())

  override def lowPriority = {
    case GetMore => {updateListeners(); this ! GetMore}
    case None => ()
  }
}


class CometSearch extends CometActor with CometListener {

  def registerWith = CometSearcher.search(term).getOrElse(new FailSearcher)

  var term = ""

  def updateTerm(newTerm: String) = {
    registerWith ! RemoveAListener(this)
    term = newTerm
    registerWith ! AddAListener(this)
    this ! Update
  }

  override def defaultPrefix = Full("search")

  def renderAResult(mem: types.Member) = <dt>{mem.in.obj.open_!.toXhtml}</dt> ++ <dd>{mem.toXhtml}</dd>

  def renderResults(res: List[(query.Query, List[types.Member])]): NodeSeq = {
    res.map {
      case (q, rs) => {
        val rendered: NodeSeq = rs match {
          case Nil => <dt>No results</dt><dd>No results for this query</dd>
          case _ => rs.map(renderAResult).foldLeft(NodeSeq.Empty)(_++_)
        }
        <dt>{q.toString}</dt><dd><dl>{rendered}</dl></dd>
      }
    }.foldLeft(NodeSeq.Empty)(_++_)  match {
      case NodeSeq.Empty => <dt>No results</dt> ++ <dd>No results are available for your query.</dd>
      case x => x
    }
  }

  var resultList: List[(query.Query, List[types.Member])] = Nil

  val input = ajaxText(term, {newTerm => updateTerm(newTerm); Noop})

  def render = bind("search", "results" -> <div id="results">{renderResults(resultList)}</div>, "input" -> input)

  ActorPing.schedule(this, Ping, 10000L)

  override def mediumPriority : PartialFunction[Any, Unit] = {
    case Update => {
      partialUpdate(SetHtml("results", renderResults(resultList)))
    }
    case Ping => {
      this ! Update
      ActorPing.schedule(this, Ping, 10000L)
    }
    case Results(newRes) => resultList = newRes; this ! Update
  }
}

case object Update
case object Ping
case class Results(members: List[(query.Query, List[types.Member])])
