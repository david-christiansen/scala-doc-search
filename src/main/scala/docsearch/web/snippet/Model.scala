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

import docsearch.types
import docsearch.utils._

/**
 * Implemented like the other ones from Lift library
 */
case class JqAfter(content: NodeSeq) extends JsExp with JsMember {
  override val toJsCmd = "after(" + fixHtml("inline", content) + ")"
}

case class JsMem(name: String) extends JsExp with JsMember {
  override val toJsCmd = name
}

class ModelView {

  def packageList(html: NodeSeq): NodeSeq = {
    val rootPackage = types.Class.find(
      By(types.Class.name, "_root_"),
      NullRef(types.Class.in),
      By(types.Class.tlt, types.TopLevelType.Package)
    )
    rootPackage match {
      case Full(p) => packageList(p)(html)
      case _ => Text("Root package missing")
    }
  }

  def packageList(lookIn: types.Class)(html:NodeSeq): NodeSeq = {
    val contains = types.Class.findAll(
      By(types.Class.in, lookIn),
      ByList(
        types.Class.tlt,
        List(
          types.TopLevelType.Package,
          types.TopLevelType.Object,
          types.TopLevelType.Class,
          types.TopLevelType.Trait
        )
      ),
      OrderBy(types.Class.name, Ascending)
    )

    val members = types.Member.findAll(
      By(types.Member.in, lookIn),
      OrderBy(types.Member.name, Ascending)
    )

    def toggleChildren(parent: types.Class)(contents: NodeSeq): NodeSeq = {
      val id: String = nextFuncName
      val childId: String = nextFuncName

      a(contents, "id" -> id) {
        val children = packageList(parent)(html)
        val childData = <ul id={childId}>{children}</ul>
        JsIf(
          JsGt(JqId(childId)~>JsMem("length"), JsRaw("0")),
          JqId(childId)~>JqRemove(),
          JqId(id)~>JqAfter(childData)
        )
      }
    }

    def memberString(m: types.Member): NodeSeq = {
      val memType = m.memType.toString
      val name = m.name.is
      val args = m.getArgLists map {
        alist: List[types.Arg] =>
        alist.map(arg => arg.name.is + ": " + arg.typ.obj.map(_.toString).openOr("NO TYPE")).
          mkString("(", ", ", ")")
      } mkString
      val resType = m.resultType.obj.map(_.name).openOr("NO TYPE!")
      val typeParams = {
        if (m.typeParams.length == 0) ""
        else "[" + m.typeParams.map(_.name).mkString(",") + "]"
      }

      Text(memType + " " + name + typeParams + args + ": " + resType)
    }

    contains.flatMap((p: types.Class) =>
      bind("item", html,
           "name" -> toggleChildren(p)(p.toXhtml))
    ) ++
    members.flatMap((m: types.Member) =>
      bind("item", html, "name" -> m.toXhtml)
    )
  }

}
