package docsearch.types.nondb
import docsearch.types.MemType._
import scala.xml._

sealed abstract class Type {
  def toXML: NodeSeq
}

object Type {
  def fromXML(xml: NodeSeq): Type = error("Later")
}

case class Tuple(elems: List[Type]) extends Type {
  def toXML = <tuple>{for ((elt, idx) <- elems.zipWithIndex) yield <elem index={idx.toString}>{elt.toXML}</elem>}</tuple>
}

case class Function(args: List[Type], res: Type) extends Type {
  def toXML = <function><args></args><result>{res.toXML}</result></function>
}

case class TypeApp(typeOp: Type, typeArgs: List[Type]) extends Type {
  def toXML = <app>
                <op>{typeOp.toXML}</op>
                <args>{for ((arg, idx) <- typeArgs.zipWithIndex) yield <arg index={idx.toString}>{arg.toXML}</arg>}</args>
              </app>
}
