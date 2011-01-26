package docsearch.types.nondb
import docsearch.types.MemType._
import scala.xml._

trait CanBeXML {
  def toXML: NodeSeq
}

trait Deserializer[T] {
  def fromXML(xml: NodeSeq): Option[T]
}

sealed abstract class MemType extends CanBeXML
case object Def extends MemType {
  def toXML = <def/>
}
case object Var extends MemType {
  def toXML = <var/>
}
case object Val extends MemType {
  def toXML = <val/>
}
case object LazyVal extends MemType {
  def toXML = <lazyVal/>
}
object MemType {
  def fromXML(xml: NodeSeq): MemType =
    xml match {
      case <def/> => Def
      case <var/> => Var
      case <val/> => Val
      case <lazyVal/> => LazyVal
      case _ => error(xml.toString + " is not a valid MemType")
    }
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


case class Member(in: Container, memType: MemType, name: String, id: String, args: List[List[(String, Type)]], resType: Type) extends CanBeXML {
  def toXML =
    <member in={in.id} name={name} id={id}>
      <memType>{memType.toXML}</memType>
      <args> {
        for (argList <- args)
        yield <argList> {
          for ((name, typ) <- argList)
          yield <arg name={name}>{typ.toXML}</arg>
        } </argList>
      } </args>
    </member>
}

object Member extends Deserializer[Member] {
  def fromXML(xml: NodeSeq): Some[Member] =
    xml match {
      case <member><memType>{mType}</memType><args>{argLists}</args></member> => {
        //TODO: Extract more data!
        Some(Member(null, null, null, null, null, null))
      }
    }
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

trait Instantiable[T <: NonRootContainer] {
  def id: String
}

case class ClassPlaceholder(in: Container, name: String, typeArgs: List[(String, Kind)]) extends NonRootContainer with Instantiable[ClassPlaceholder] {
  def toXML =
    <classPlaceholder id={id} name={name}>
      <args>{
        typeArgs map {case (arg, k) => <arg>{arg}</arg>}
      }</args>
    </classPlaceholder>
}

case class Class(in: Container, name: String, typeArgs: List[(String, Kind)], inherits: Type) extends NonRootContainer with Instantiable[Class] {
  def toXML =
    <class id={id} name={name}>
      <args>{
        typeArgs map {case (arg, k) => <arg>{arg}</arg>}
      }</args>
      <inherits>{inherits.toXML}</inherits>
    </class>
}

case class Trait(in: Container, name: String, typeArgs: List[(String, Kind)], inherits: Type) extends NonRootContainer with Instantiable[Trait] {
  def toXML =
    <trait id={id} name={name}>
      <args>{typeArgs map {case (arg, k) => <arg>{arg}</arg>}}</args>
      <inherits>{inherits.toXML}</inherits>
    </trait>
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

case class TypeVarDeref(name: String) extends Type {
  def toXML = <typeVar name={name}/>
}

case class TraitComposition(base: Type, traits: List[Type]) extends Type {
  def toXML = <with><base>{base.toXML}</base><traits>{traits.map(_.toXML)}</traits></with>
}

case class InstanceOf[T <: NonRootContainer](what: Instantiable[T]) extends Type {
  def toXML = <instance of={what.id} />
}
