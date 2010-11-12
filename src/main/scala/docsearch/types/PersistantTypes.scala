package docsearch.types

import net.liftweb.mapper._
import net.liftweb.common.{Box,Full,Empty,Failure,ParamFailure}


object Variance extends Enumeration {
  type Variance = Value
  val Covariant = Value("covariant")
  val Contravariant = Value("contravariant")
  val Invariant = Value("invariant")
}

import Variance._

object TypeEnum extends Enumeration {
  type TypeEnum = Value
  val Object = Value("object")
  val Package = Value("package")
  val Class = Value("class")
  val Trait = Value("trait")
}

import TypeEnum._

object TypeType extends Enumeration {
  type TypeEnum = Value
  val Tuple = Value("tuple")
  val Function = Value("function")
  val Method = Value("method")
  val TypeVar = Value("type variable")
  val InstanceOf = Value("Instance of")
}

import TypeType._

object MemType extends Enumeration {
  type MemType = Value
  val Def = Value("def")
  val Val = Value("val")
  val Var = Value("var")
}

import MemType._

//Class
class Class extends LongKeyedMapper[Class] with IdPK {
  def getSingleton = Class
  object name extends MappedString(this, 100)
  object in extends MappedLongForeignKey(this, Package)
  object parent extends MappedLongForeignKey(this, Class)
  //members: query members to see who is in this class?
  object typ extends MappedEnum[Class, TypeEnum.type](this, TypeEnum)
  object inherits extends MappedLongForeignKey(this, Class) //can be more than 1
  object typeParam extends MappedLongForeignKey(this, TypeParam)
  object constructor extends MappedLongForeignKey(this, Arg)
}

object Class extends Class with LongKeyedMetaMapper[Class] 

//Type Param
class TypeParam extends LongKeyedMapper[TypeParam] with IdPK with OneToMany[Long, TypeParam] {
  def getSingleton = TypeParam
  object name extends MappedString(this, 100)
  object order extends MappedInt(this)
  
  object parent extends MappedLongForeignKey(this, TypeParam) // NULL if top-level
  object kind extends MappedLongForeignKey(this, Kind) {
    //override def defaultValue: Kind = Kind.typ
  }

  object params extends MappedOneToMany(TypeParam, TypeParam.parent, OrderBy(TypeParam.order, Ascending))
  
  def isTop: Boolean = {
    this.parent.obj match {
      case Full(_) => false
      case _ => true
    }
  }
  
  override def toString() = {
    name.is +
    (this.params.all match {
      case Nil => ""
      case ps => "[" + ps.map(_.toString).mkString(",") + "]" 
    }) +
    (if (this.isTop) {
      " : " + 
      (kind.obj match {
        case Full(k) => k.toString
        case _ => "error finding kind"
      })
    } else "")
  }
}

object TypeParam extends TypeParam with LongKeyedMetaMapper[TypeParam]

//Type
class Type extends LongKeyedMapper[Type] with IdPK {
  def getSingleton = Type
  object typeType extends MappedEnum[Type, TypeType.type](this, TypeType) // Type of Type
  object name extends MappedString(this, 100)             //base case
  object res extends MappedLongForeignKey(this, Type)     //method or function
  object argsOrElements extends MappedLongForeignKey(this, Arg) //Methods and functions and tuples
  object typeParam extends MappedLongForeignKey(this, TypeParam)  //Instance of 
  object type1 extends MappedLongForeignKey(this, Class) //Instance of
  
}

object Type extends Type with LongKeyedMetaMapper[Type] 

//Arg
class Arg extends LongKeyedMapper[Arg] with IdPK {
  def getSingleton = Arg
  object name extends MappedString(this, 100)
  object typ extends MappedLongForeignKey(this, Type)
}

object Arg extends Arg with LongKeyedMetaMapper[Arg] 

//Kind
class Kind extends LongKeyedMapper[Kind] with IdPK {
  def getSingleton = Kind

  object from extends MappedLongForeignKey(this,Kind)
  object to extends MappedLongForeignKey(this,Kind)

  override def toString() = {
    if (!this.isValid) "errKind"
    else if (this.isNullary) "*"
    else {
      val fromK = this.from.obj.open_!
      val toK = this.to.obj.open_!
      (if (fromK.isNullary) fromK.toString else "(" + fromK.toString + ")") +
      " -> " +
      (if (toK.isNullary) toK.toString else "(" + toK.toString + ")")
    }
  }

  def isValid(): Boolean = 
    (this.from.obj.isEmpty == this.to.obj.isEmpty)

  def isNullary(): Boolean =
    (this.from.obj.isEmpty && this.to.obj.isEmpty)
}

object Kind extends Kind with LongKeyedMetaMapper[Kind] {
  // kind *
  def nullary() = 
    Kind.find(By(Kind.from, Empty), By(Kind.to, Empty)) openOr Kind.create.saveMe
  // kind _ --> _
  def arrow(f: Kind, t: Kind): Kind = {
    require(f.isValid && t.isValid)
    Kind.find(By(Kind.from, f), By(Kind.to, t)).openOr(
      Kind.create.to(t).from(f).saveMe
    )
  }

  // Construct the kind of a type constructor based on its parameters
  def getKind(tps: List[TypeParam]): Kind =
    tps match {
      case Nil     => nullary
      case p :: ps => arrow(p.kind.obj.open_!, getKind(ps))
    }
}

//Member
class Member extends LongKeyedMapper[Member] with IdPK {
  def getSingleton = Member
  object in extends MappedLongForeignKey(this, Class)
  object typeParams extends MappedLongForeignKey(this, TypeParam)
  object memType extends MappedEnum[Member,MemType.type](this, MemType)
  object resultType extends MappedLongForeignKey(this, Type)
  object args extends MappedLongForeignKey(this, Arg) //one to many 
}

object Member extends Member with LongKeyedMetaMapper[Member] 

//Package
class Package extends LongKeyedMapper[Package] with IdPK {
  def getSingleton = Package
}

object Package extends Package with LongKeyedMetaMapper[Package] 

