/* package docsearch.types.nondb

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

object ContainerGen {
  import TypeGen._

  val genRootPackage = value(RootPackage)

  val genNonRoot = oneOf(genObject, genPackage)

  def genPackage: Gen[NonRootContainer] =
    for {
      parent <- oneOf(genRootPackage, genPackage)
      name <- alphaStr
    } yield Package(parent, name)

  val genObject: Gen[Object] =
    for {
      parent <- oneOf(genPackage, genObject)
      name <- alphaStr
    } yield Object(parent, name)

  val genContainer: Gen[Container] = oneOf(genRootPackage, genNonRoot, genClass)

  val genInstantiable: Gen[Instantiable[_ <: NonRootContainer]] = oneOf(genClass, genTrait)

  val genClass: Gen[Class] =
    for {
      in <- genContainer
      name <- alphaStr
    } yield Class(in, name, List(), InstanceOf(IAnyRef()))

  val genTrait: Gen[Trait] =
    for {
      in <- genContainer
      name <- alphaStr
    } yield Trait(in, name, List(), InstanceOf(IAnyRef()))
}

object TypeGen {
  import ContainerGen._

  val genTuple: Gen[Type] = sized { size =>
    for (elems <- listOfN(size, genType)) yield Tuple(elems)
  }

  val genFunction: Gen[Type] = sized { size =>
    for {
      args <- listOfN(size, genType)
      res <- genType
    } yield Function(args, res)
  }

  val genTypeApp: Gen[Type] = sized { size =>
    for {
      op <- genType
      args <- listOfN(size, genType)
    } yield TypeApp(op, args)
  }

  val genTypeVarDeref: Gen[Type] =
    for {
      name <- alphaStr
    } yield TypeVarDeref(name)

  val genTraitComposition: Gen[Type] = sized { size =>
    for {
      base <- genType
      traits <- listOfN(size, genType)
    } yield TraitComposition(base, traits)
  }

  val genInstanceOf: Gen[Type] = for {clas <- genInstantiable} yield InstanceOf(clas)

  def genType: Gen[Type] = oneOf(genTuple, genFunction, genTypeApp, genTypeVarDeref, genTraitComposition, genInstanceOf)
}

object TypeSerializationSpec extends Properties("TypeSerialization") {
/*  property("typeSerializationWorks") = Prop.forAll {
    t: Type => Type.fromXML(t.toXML) == t
  }*/
}
*/
