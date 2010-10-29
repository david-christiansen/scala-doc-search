package docsearch.types
import docsearch.query.QMemType._

object Variance extends Enumeration {
  type Variance = Value
  val Covariant = Value("covariant")
  val Contravariant = Value("contravariant")
  val Invariant = Value("invariant")
}

object ClassOrTrait extends Enumeration {
  type ClassOrTrait = Value
  val Class = Value("class")
  val Trait = Value("trait")
}
import Variance._
import ClassOrTrait._

case class Arg(name: String, typ: Type)

case class Member(in: Type, typeParams: List[Kind], memType: QMemType, resultType: Type, args: List[List[Arg]])

abstract sealed class SearchContainer

abstract sealed class Package extends SearchContainer {
  def path(): List[Package]
}
case object PathRoot extends Package {
  def path() = List(this)
}
case class NamedPackage(in: Package, name: String) extends Package {
  def path() = in.path ++ List(this)
}



abstract sealed class Kind
case object * extends Kind
case class -->(from: Kind, to: Kind, variance: Variance) extends Kind

abstract sealed class Type extends SearchContainer
case class Tuple(elements: List[Type]) extends Type
case class Func(args: List[Type], res: Type) extends Type
case class Class(name: String, 
                 in: Either[Package, Class], 
                 members: List[Member], 
                 inherits: List[Class], 
                 constructor: List[Arg],
                 typeParams: List[(String, Kind)],
                 classOrTrait: ClassOrTrait) extends Type
case class Method(args: List[List[Arg]], res: Type) extends Type
case class TypeVar(name: String) extends Type
case class TypeApp(type1: Class, type2: Type) extends Type


