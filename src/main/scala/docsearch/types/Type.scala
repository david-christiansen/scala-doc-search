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

case class Member(in: TypeDef, typeParams: List[Kind], memType: QMemType, resultType: TypeDef, args: List[List[Arg]])

abstract sealed class Package {
  def path(): List[Package]
}
case object PathRoot extends Package {
  def path() = List(this)
}
case class NamedPackage(parent: Package, name: String) extends Package{
  def path() = parent.path ++ List(this)
}



abstract sealed class Kind
case object * extends Kind
case class -->(from: Kind, to: Kind, variance: Variance) extends Kind

abstract sealed class TypeDef
case class Tuple(elements: List[TypeDef]) extends TypeDef
case class Func(args: List[TypeDef], res: TypeDef) extends TypeDef
case class Class(typeArgs: List[Kind], 
                  name: String, 
                  in: Either[Package, Class], 
                  members: List[Member], 
                  inherits: List[Class], 
                  constructor: List[Arg], 
                  classOrTrait: ClassOrTrait)
case class Method(args: List[List[Arg]], res: TypeDef) extends TypeUse

abstract sealed class TypeUse
case class TypeApp(t: TypeDef, args: List[TypeUse]) extends TypeUse
object TypeUse {
  implicit def useType(t:TypeDef):TypeUse = TypeApp(t, List())
}

case class Arg(name: String, typ: TypeUse)
