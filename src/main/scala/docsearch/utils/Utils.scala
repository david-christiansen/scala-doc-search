package docsearch
import scala.xml.{NodeSeq, Text, Node}
import net.liftweb.common._

package object utils {
  class NodePimp(nodes: NodeSeq) {
    def mkNodes(start: Node, sep: Node, end: Node): NodeSeq = {
      start ++ this.mkNodes(sep) ++ end
    }

    def mkNodes(sep: Node): NodeSeq = {
      if (nodes.length == 0) nodes
      else nodes.head ++ nodes.tail.flatMap(sep ++ _)
    }
    def parenList = this.mkNodes(Text("("), Text(", "), Text(")"))
  }
  implicit def pimpMyNodes(nodes: NodeSeq): NodePimp = new NodePimp(nodes)

  class StringPimp(str: String) {
    def parseInt: Box[Int] = try {
      Full(str.toInt)
    } catch {
      case e: java.lang.NumberFormatException => Empty
    }
  }
  implicit def pimpMyStrings(str: String): StringPimp = new StringPimp(str)
}
