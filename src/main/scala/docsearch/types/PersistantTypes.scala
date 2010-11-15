package docsearch.types

import net.liftweb.mapper._
import net.liftweb.common.{Box,Full,Empty,Failure,ParamFailure}

import scala.tools.nsc.doc.{model, Universe}


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

//Class, Object, Trait, Package
class Class extends LongKeyedMapper[Class] with IdPK with OneToMany[Long, Class] with ManyToMany {
  def getSingleton = Class
  object entityToString extends MappedString(this, 200)
  object name extends MappedString(this, 100)
  object in extends MappedLongForeignKey(this, Class)
  object memberClasses extends MappedOneToMany(Class, Class.in, OrderBy(Class.name, Ascending))
  object members extends MappedOneToMany(Member, Member.in, OrderBy(Member.id, Ascending))
  object typ extends MappedEnum[Class, TypeEnum.type](this, TypeEnum)
  object children extends MappedManyToMany(Inheritance, Inheritance.children, Inheritance.parents, Class) 
  object parents extends MappedManyToMany(Inheritance, Inheritance.parents, Inheritance.children, Class)
  object typeParams extends MappedManyToMany(ClassTypeParam, ClassTypeParam.classes, ClassTypeParam.typeParams, TypeParam)
  object constructor extends MappedLongForeignKey(this, Arg)

  def path = this.in.obj.map(List(_)).openOr(List()) ++ List(this)
  
  override def toString() = {
    //FIXME add type params and such
    path.map(_.name.is).mkString(".")
  }
}

object Class extends Class with LongKeyedMetaMapper[Class] {
  def createClass(entityToString: String, name: String, in: String) ={     
    Class.find(By(Class.entityToString, entityToString)) openOr {
      var clas = Class.create.entityToString(entityToString).
        name(name).
        typ(TypeEnum.Class).
        in(Class.find(By(Class.entityToString, in)).openOr(error("Could not find " + in)))
      /*not quite working yet
      for (p <- parents) p match {
        case c: model.Class => clas.parents += Class.find(By(Class.entityToString, c.toString)).openOr(error("Could not find " + c.toString))
        case o: model.Object => clas.parents += Class.find(By(Class.entityToString, o.toString)).openOr(error("Could not find " + o.toString))
        case t: model.Trait => clas.parents += Class.find(By(Class.entityToString, t.toString)).openOr(error("Could not find " + t.toString))
        case _ => ()
      }*/
      clas.saveMe
    }
  }
  
  def createRelationships(entityToString: String, parents: List[model.TemplateEntity]) = {
    var clas = Class.find(By(Class.entityToString, entityToString)) openOr (error("Could not find " + entityToString))
    for (p <- parents) p match {
      case c: model.Class => clas.parents += Class.find(By(Class.entityToString, c.toString)).openOr(error("Could not find " + c.toString))
      case o: model.Object => clas.parents += Class.find(By(Class.entityToString, o.toString)).openOr(error("Could not find " + o.toString))
      case t: model.Trait => clas.parents += Class.find(By(Class.entityToString, t.toString)).openOr(error("Could not find " + t.toString))
      case _ => ()
    }
    clas.save
  }

  def createTrait(entityToString: String, name: String, in: String) = {
    Class.find(By(Class.entityToString, entityToString)) openOr
      Class.create.entityToString(entityToString).
        name(name).
        typ(TypeEnum.Trait).
        in(Class.find(By(Class.entityToString, in)).openOr(error("Could not find " + in))).
        saveMe
  }
  
  def createObject(entityToString: String,
                    name: String,
                    in: String) = {
    Class.find(By(Class.entityToString, entityToString)) openOr
      Class.create.
        entityToString(entityToString).
        name(name).
        typ(TypeEnum.Object).
        in(Class.find(By(Class.entityToString, in)) openOr error("Could not find " + in)). // we can assume the parent is there
        saveMe
  }
  
  def createRootPackage(asString: String) = {
    Class.find(By(Class.entityToString, asString)) openOr 
      Class.create.entityToString(asString).name(asString).typ(TypeEnum.Package).saveMe
    }
    

  def createPackage(entityToString: String, 
                    name: String, 
                    in: model.DocTemplateEntity): Class = {
    Class.find(By(Class.entityToString, entityToString)) openOr 
      Class.create.
        entityToString(entityToString).
        name(name).
        typ(TypeEnum.Package).
        in(
          Class.find(By(Class.entityToString, in.toString)) openOr error("Couldn't find " + in.toString) //Just require that parent has been created
      ).saveMe
    }
}

//Inheritance middle table
class Inheritance extends Mapper[Inheritance] {
  def getSingleton = Inheritance  
  object children extends MappedLongForeignKey(this, Class)
  object parents extends MappedLongForeignKey(this, Class)
}

object Inheritance extends Inheritance with MetaMapper[Inheritance]

//Class Type Param middle table
class ClassTypeParam extends Mapper[ClassTypeParam] {
  def getSingleton = ClassTypeParam  
  object classes extends MappedLongForeignKey(this, Class)
  object typeParams extends MappedLongForeignKey(this, TypeParam)
}

object ClassTypeParam extends ClassTypeParam with MetaMapper[ClassTypeParam]


//Type Param
class TypeParam extends LongKeyedMapper[TypeParam] with IdPK with OneToMany[Long, TypeParam] with ManyToMany {
  def getSingleton = TypeParam
  object name extends MappedString(this, 100)
  object order extends MappedInt(this)
  object kind extends MappedLongForeignKey(this, Kind)
  object classes extends MappedManyToMany(ClassTypeParam, ClassTypeParam.typeParams, ClassTypeParam.classes, Class)
  object params extends MappedOneToMany(TypeParam, TypeParam.parent, OrderBy(TypeParam.order, Ascending))
  object parent extends MappedLongForeignKey(this, TypeParam) // NULL if top-level
  object typ extends MappedLongForeignKey(this, Type)
  
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
class Type extends LongKeyedMapper[Type] with IdPK with OneToMany[Long, Type] with ManyToMany {
  def getSingleton = Type
  object typeType extends MappedEnum[Type, TypeType.type](this, TypeType) // Type of Type
  object name extends MappedString(this, 100)             //typeVar
  object res extends MappedLongForeignKey(this, Type)     //method or function   
  object funcArgs extends MappedOneToMany(Arg, Arg.typ, OrderBy(Arg.id, Ascending)) //list of args
  object methodArgs extends MappedOneToMany(Type, Type.typeFK, OrderBy(Type.id, Ascending)) //Methods (list of funcArgs)   
  
  object elements extends MappedOneToMany(Type, Type.typeFK, OrderBy(Type.id, Ascending)) //tuples
  object typeFK extends MappedLongForeignKey(this, Type) //foreign key for self referencing   
  object typeParam extends MappedOneToMany(TypeParam, TypeParam.typ, OrderBy(TypeParam.id, Ascending))  //Instance of 
  object instanceOf extends MappedLongForeignKey(this, Class) //Instance of
  
  
  //TODO validation
  def isTuple: Boolean = 
    res.obj.isEmpty &&
    funcArgs.length == 0 &&
    methodArgs.length == 0 &&
    elements.length > 0 &&
    typeParam.length == 0 &&
    instanceOf.obj.isEmpty
  def isFunc = ""
  def isMethod = ""
  def isTypeVar = ""
  def isInstanceOf = ""
}

object Type extends Type with LongKeyedMetaMapper[Type] 


//TypeArg middle table
class TypeArg extends Mapper[TypeArg]{
  def getSingleton = TypeArg
  object types extends MappedLongForeignKey(this, Type)
  object args extends MappedLongForeignKey(this, Arg)
}

object TypeArg extends TypeArg with MetaMapper[TypeArg]

//Arg
class Arg extends LongKeyedMapper[Arg] with IdPK with ManyToMany{
  def getSingleton = Arg
  object name extends MappedString(this, 100)
  object typ extends MappedLongForeignKey(this, Type)
  object member extends MappedLongForeignKey(this, Member)
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
class Member extends LongKeyedMapper[Member] with IdPK with OneToMany[Long, Member]{
  def getSingleton = Member
  object in extends MappedLongForeignKey(this, Class)
  object typeParams extends MappedLongForeignKey(this, TypeParam)
  object memType extends MappedEnum[Member,MemType.type](this, MemType)
  object resultType extends MappedLongForeignKey(this, Type)
  object args extends MappedOneToMany(Arg, Arg.member, OrderBy(Arg.id, Ascending)) //one to many 
  
}

object Member extends Member with LongKeyedMetaMapper[Member] 

