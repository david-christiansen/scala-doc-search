package docsearch.web
package snippet

import scala.xml.{Text, NodeSeq}

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util.Helpers._

import docsearch.types

class ModelView {

  def packageList(html: NodeSeq): NodeSeq = {
    val rootPackage = types.Class.find(
      By(types.Class.name, "_root_"),
      NullRef(types.Class.in),
      By(types.Class.typ, types.TypeEnum.Package)
    )
    rootPackage match {
      case Full(p) => bind("package", html, "name" -> p.name.is)
      case _ => Text("Root package missing")
    }
  }

}
