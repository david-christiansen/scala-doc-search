package docsearch.types


sealed abstract class PathComponent
case class Package(name: String) extends PathComponent {
  override def toString() = name
}
case class TypeName(name: String) extends PathComponent {
  override def toString() = name
}
case object Super extends PathComponent {
  override def toString() = "super"
}
case object This extends PathComponent {
  override def toString() = "this"
}

case class Path(components: List[PathComponent]) {
  override def toString() = {
    if (components.length < 1) "ε"
    else components map {_.toString} mkString "."
  }

  def +(another: PathComponent) = 
    copy(components=(components ++ List(another)))

  def ::(another:PathComponent) = 
    copy(components=(another::components))
}

sealed abstract class SimpleType
case class SingletonType(path: Path) extends SimpleType {
  override def toString() = path.toString
} 
case class TypeProjection(typ: SimpleType, id: String) extends SimpleType {
  override def toString() = typ.toString + "#" + id
}
case class ParamType(typ: SimpleType, args: List[SimpleType]) extends SimpleType {
  override def toString() = typ.toString + "["+ args.mkString(", ") + "]"
}
case class TupleType(types: List[SimpleType]) extends SimpleType {
  override def toString() = "("+ types.mkString(", ") + ")"
}

case class Annotation(name: String) {
  override def toString() = "@" + name
}
case class AnnotType(typ: SimpleType, annotations: List[Annotation])

case class CompoundType(typ: AnnotType, wth: List[AnnotType] /* Add refinement here later */)

case class InfixType(name: String, typ1: CompoundType, typ2: CompoundType)

sealed abstract class FunctionArgs
case class InfixArg(typ: InfixType) extends FunctionArgs
case class ArgList(params: List[ParamType]) extends FunctionArgs

sealed abstract class ExistentialMod
case class ExistentialModType() extends ExistentialMod /* Add typedecl here later */
case class ExistentialModVal() extends ExistentialMod /* add val decl here later */


sealed abstract class Type
case class FunctionType (args: FunctionArgs, res: Type) extends Type
case class ExistentialType(typ: InfixType, existentials: List[ExistentialMod])

