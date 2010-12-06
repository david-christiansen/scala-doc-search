package docsearch
import scala.xml.{NodeSeq, Text, Node}

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

  
}
