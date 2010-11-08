/*
package docsearch.types
import docsearch.types.MemType._



object ClassOrTrait extends Enumeration {
  type ClassOrTrait = Value
  val Class = Value("class")
  val Trait = Value("trait")
}
import ClassOrTrait._

case class Arg(name: String, typ: Type)

abstract sealed class Package {
  def path(): List[Package]
}
case object PathRoot extends Package {
  def path() = List(this)
}
case class NamedPackage(in: Package, name: String) extends Package {
  def path() = in.path ++ List(this)
}

case class Object(in: Package, name: String, members: List[Member]) extends Package {
  def path() = in.path ++ List(this)
}

case class Class(name: String, 
                 in: Package,
                 members: List[Member], 
                 inherits: List[Class], 
                 constructor: List[Arg],
                 typeParams: List[(String, Kind)],
                 classOrTrait: ClassOrTrait) extends Package {
  def path() = in.path ++ List(this)
}

abstract sealed class Kind {
  def --> (k: Kind) = new -->(this, k)
}
case object * extends Kind {
  override def toString = "*"
}
case class -->(from: Kind, to: Kind) extends Kind {
  override def toString = "(" + from.toString + " --> " + to.toString + ")"
}

abstract sealed class Type
case class Tuple(elements: List[Type]) extends Type
case class Func(args: List[Type], res: Type) extends Type
case class Method(args: List[List[Arg]], res: Type) extends Type
case class TypeVar(name: String) extends Type
case class InstanceOf(type1: Class, typeParams: List[Type]) extends Type

*/
