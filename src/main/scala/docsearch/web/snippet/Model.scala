package docsearch.web
package snippet

import scala.xml.{Text, NodeSeq}

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
      ByList(types.Class.tlt, List(types.TopLevelType.Package, types.TopLevelType.Object, types.TopLevelType.Class, types.TopLevelType.Trait)),
      OrderBy(types.Class.name, Ascending)
    )
    
    def toggleChildren(parent: types.Class)(contents: NodeSeq): NodeSeq = {
      val id: String = nextFuncName
      val childId: String = nextFuncName
      
      a(contents, "id" -> id) {
        val children = packageList(parent)(html)
        val childList = <ul id={childId}>{children}</ul>
        JsIf(
          JsGt(JqId(childId)~>JsMem("length"), JsRaw("0")), 
          JqId(childId)~>JqRemove(),
          JqId(id)~>JqAfter(childList)
        )
      }
    }

    def icon(c: types.Class): NodeSeq = c.tlt.is match {
      case types.TopLevelType.Package => Text("[p]")
      case types.TopLevelType.Object => Text("[o]")
      case types.TopLevelType.Class => Text("[c]")
      case types.TopLevelType.Trait => Text("[t]")
      case _ => Text ("[]")
    }

    contains.flatMap((p: types.Class) => 
      bind("package", html, 
           "icon" -> icon(p),
           "name" -> toggleChildren(p)(Text(p.name.is)))
    )
  }

}
