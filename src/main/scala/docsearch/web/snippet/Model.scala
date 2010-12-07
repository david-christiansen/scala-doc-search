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
    
    def className(c: types.Class): NodeSeq = {
      <span class="className" id={c.id.toString}>{c.name.is}</span> ++ 
      (if (c.typeParams.length > 0)
        <span class="typeParams">
          {(NodeSeq.Empty ++ c.typeParams.map {
            tp: types.TypeParam => <span class="typeParam" id={tp.id.toString}>{tp.name.is}</span>
            }).mkNodes(Text("["), Text(", "), Text("]"))
          }
        </span>
      else NodeSeq.Empty)
    }

    def inheritsFrom(c: types.Class): NodeSeq = 
      if (c.parents.length > 0)
        Text(" extends ") ++ className(c.parents.head) ++ 
        (NodeSeq.Empty ++ c.parents.tail.flatMap((c: types.Class) => Text(" with ") ++ className(c)))
      else NodeSeq.Empty

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

    def classType(c: types.Class): NodeSeq = c.tlt.is match {
      case types.TopLevelType.Package => Text("package")
      case types.TopLevelType.Object => Text("object")
      case types.TopLevelType.Class => Text("class")
      case types.TopLevelType.Trait => Text("trait")
      case _ => Text ("[]")
    }

    def argNodes(arg: types.Arg): NodeSeq = 
      <span class="arg">
        {if (arg.isImplicit) Text("Implicit") else NodeSeq.Empty}
        {Text(arg.name.is)}: 
        {arg.typ.map(_.toXhtml).openOr(Text("ERROR"))}
      </span>

    def flattenNodes(ns: Seq[NodeSeq]): NodeSeq = 
      ns.foldLeft(NodeSeq.Empty) {(a: NodeSeq, b: NodeSeq) => a ++ b}

    def memberNodes(m: types.Member): NodeSeq = {
      val memType = m.memType.toString
      val name = Text(m.name.is)
      val args: NodeSeq = {
        val aLists = m.getArgLists flatMap {
          argList => <span class="argList">{flattenNodes(argList.map(argNodes)).parenList}</span>
        }
        <span class="argLists">{flattenNodes(aLists)}</span>
      }
      val resType = m.resultType.obj.map(_.toXhtml).openOr("NO TYPE!")
      val typeParams = {
        if (m.typeParams.length == 0) NodeSeq.Empty
        else {
          val params: NodeSeq = m.typeParams map {
            p: types.TypeParam => <span class="typeParam" id={p.id.toString}>{p.name.is}</span>
          }
          <span class="typeParams">{params.mkNodes(Text("["), Text(", "), Text("]"))}</span>
        }
      }
      
      <span class="member">
        {Text(memType)}
        {Text(" ")}
        <span class="memName">{name}</span>
        {typeParams}
        {args}:  {resType}
      </span>
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
           "name" -> toggleChildren(p)(classType(p) ++ Text(" ") ++ className(p) ++ inheritsFrom(p)))
    ) ++
    members.flatMap((m: types.Member) =>
      bind("item", html, "name" -> memberNodes(m))
    )
  }

}
