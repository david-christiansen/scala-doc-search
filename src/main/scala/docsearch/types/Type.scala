package docsearch.types.nondb
import docsearch.types.MemType._
import scala.xml._

trait CanBeXML {
  def toXML: NodeSeq
}



sealed abstract class Kind extends CanBeXML
case object * extends Kind {
  def --> (to: Kind) = new -->(this, to)
  def toXML = <starKind/>
}
case class -->(from: Kind, to: Kind) extends Kind {
  override def toString = from.toString + " --> " + to.toString
  def toXML = <arrKind>{from.toXML}{to.toXML}</arrKind>
}



sealed abstract class Container extends CanBeXML {
  def id: String
}

case object RootPackage extends Container {
  def toXML = <rootPackage/>
  def id = "_root_"
}

sealed abstract class NonRootContainer extends Container {
  val in: Container
  val name: String
  def id = in.id + "." + this.name
}

case class Object(in: Container, name: String) extends NonRootContainer {
  def toXML = <object id={id} name={name}/>
}

case class Package(in: Container, name: String) extends NonRootContainer {
  def toXML = <package id={id} name={name}/>
}

case class Class(in: Container, name: String, typeArgs: List[(String, Kind)]) extends NonRootContainer {
  def toXML = <class id={id} name={name}>{typeArgs map {case (arg, k) => <arg>{arg}</arg>}}</class>
}



sealed abstract class Type extends CanBeXML

object Type {
  def fromXML(xml: NodeSeq): Type = {
    xml match {
      case <tuple>{elems}</tuple> =>
        Tuple(elems.map(fromXML).toList)
      case <function><args>{args}</args><res>{res}</res></function> =>
        Function(args.map(fromXML).toList, fromXML(res))
      case <app><op>{op}</op><args>{args}</args></app> =>
        TypeApp(fromXML(op), args.map(fromXML).toList)
    }
  }
}

case class Tuple(elems: List[Type]) extends Type {
  def toXML = <tuple>{elems.map(_.toXML)}</tuple>
}

case class Function(args: List[Type], res: Type) extends Type {
  def toXML = <function><args>{args.map(_.toXML)}</args><result>{res.toXML}</result></function>
}

case class TypeApp(typeOp: Type, typeArgs: List[Type]) extends Type {
  def toXML = <app>
                <op>{typeOp.toXML}</op>
                <args>{
                  for ((arg, idx) <- typeArgs.zipWithIndex)
                  yield <arg index={idx.toString}>{arg.toXML}</arg>
                }</args>
              </app>
}
