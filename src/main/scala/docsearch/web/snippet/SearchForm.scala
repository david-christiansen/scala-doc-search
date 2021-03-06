package docsearch.web
package snippet

import scala.xml.{ Text, Node, NodeSeq }

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
import docsearch.utils._
import docsearch.search._

class SearchForm {
  def form(contents: NodeSeq): NodeSeq = {
    val input = <input name="q" type="text" value={S.param("q") openOr ""}></input>
    val submit = <input type="submit" value="Search!"></input>
    val widgets = bind("search", contents, "input" -> input, "submit" -> submit)
    <form method="get" action="">{widgets}</form>
  }

  def renderResults(res: List[types.Member]): NodeSeq = {
    res.map(x => <dt>{x.in.obj.open_!.toXhtml}</dt><dd>{x.toXhtml}</dd>).foldLeft(NodeSeq.Empty)(_++_)
  }

  def results(contents: NodeSeq): NodeSeq = {
    val toParse = S.param("q") openOr ""
    val start = S.param("begin") flatMap (_.parseInt) openOr 0
    val howMany = S.param("count") flatMap (_.parseInt) openOr 0

    if (toParse == "") NodeSeq.Empty
    else {
      Searcher.search(toParse) match {
        case Some(s) => {
          val results: List[types.Member] = s.findResults()
          bind("search", contents,
               "results" -> <div>Found {results.length.toString} results: <dl>{renderResults(results)}</dl></div>)
        }
        case None => bind("search", contents,
                          "results" -> Text("Couldn't parse '" + toParse + "'"))
      }
    }
  }
}

class CometSearch extends CometActor with CometListenee {
  def registerWith = CometSearcher.search(S.param("q") openOr "") getOrElse new SimpleActor[Any] {def !(param: Any) = ()}

  override def defaultPrefix = Full("search")

  def renderResults(res: List[types.Member]): NodeSeq = {
    res.map(x => <dt>{x.in.obj.open_!.toXhtml}</dt><dd>{x.toXhtml}</dd>).foldLeft(NodeSeq.Empty)(_++_)
  }

  var resultList: List[types.Member] = Nil

  val input = <input name="q" type="text" value={S.param("q") openOr ""}></input>
  val submit = <input type="submit" value="Search!"></input>

  def render = bind("results" -> renderResults(resultList), "input" -> input, "submit" -> submit)

  ActorPing.schedule(this, Update, 5000L)

  override def lowPriority : PartialFunction[Any, Unit] = {
    case Update => {
      partialUpdate(SetHtml("results", renderResults(resultList)))
      ActorPing.schedule(this, Update, 5000L)
    }
    case Results(newRes) => resultList = newRes; this ! Update
  }
}

case object Update
case class Results(members: List[types.Member])
