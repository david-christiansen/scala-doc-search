
package docsearch.types

object MemType extends Enumeration {
  type MemType = Value
  val Def = Value("def")
  val Val = Value("val")
  val Var = Value("var")
}

import MemType._

case class Member(in: Package, typeParams: List[Kind], memType: MemType, resultType: Type, args: List[List[Arg]])

