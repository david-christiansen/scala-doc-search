package docsearch.types

import docsearch.dumper.{TPParser, TypeParser, ScalaDocTypeLexer}

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

object TopLevelType extends Enumeration {
  type TopLevelType = Value
  val Object = Value("object")
  val Package = Value("package")
  val Class = Value("class")
  val Trait = Value("trait")
}

import TopLevelType._

object TypeType extends Enumeration {
  type TypeType = Value
  val Tuple = Value("tuple")
  val Function = Value("function")
  val Method = Value("method")
  val TypeVar = Value("type variable")
  val InstanceOf = Value("instance of")
  val ConcreteType = Value("concrete type")
}

import TypeType._

object MemType extends Enumeration {
  type MemType = Value
  val Def = Value("def")
  val Val = Value("val")
  val Var = Value("var")
  val LazyVal = Value("lazyval")
}

import MemType._

//Class, Object, Trait, Package
class Class extends LongKeyedMapper[Class] with IdPK with OneToMany[Long, Class] with ManyToMany {
  def getSingleton = Class
  object entityToString extends MappedString(this, 200)
  object name extends MappedString(this, 100)
  object tlt extends MappedEnum[Class, TopLevelType.type](this, TopLevelType)
  
  object typ extends MappedLongForeignKey(this, Type)
  
  object in extends MappedLongForeignKey(this, Class)
  object memberClasses extends MappedOneToMany(Class, Class.in, OrderBy(Class.name, Ascending)) //Needed for in
  
  object members extends MappedOneToMany(Member, Member.in, OrderBy(Member.id, Ascending))
  
  object children extends MappedManyToMany(Inheritance, Inheritance.children, Inheritance.parents, Class) 
  object parents extends MappedManyToMany(Inheritance, Inheritance.parents, Inheritance.children, Class)
  
  object typeParams extends MappedManyToMany(ClassTypeParam, ClassTypeParam.classes, ClassTypeParam.typeParams, TypeParam)
  //TODO Add contructor to Classes
  object constructor extends MappedLongForeignKey(this, Arg)

  def path = this.in.obj.map(List(_)).openOr(List()) ++ List(this)
  
  override def toString() = {
    //FIXME add type params and such
    path.map(_.name.is).mkString(".")
  }
}

object Class extends Class with LongKeyedMetaMapper[Class] {
  
  //creates a class, trait or object
  def createClass(dte: model.DocTemplateEntity) = {
    val tpp = new TPParser
    Class.find(By(Class.entityToString, dte.toString)) openOr {
      val clas = Class.create.entityToString(dte.toString).
        name(dte.name).
        tlt(dte match {
          case c: model.Class => TopLevelType.Class
          case o: model.Object => TopLevelType.Object
          case t: model.Trait => TopLevelType.Trait
          case _ => error("Got something that wasn't a trait class or object in createClass'")
        }).
        in(Class.find(By(Class.entityToString, dte.inTemplate.toString)).openOr(error("Could not find " + in))
        ).saveMe
        dte.typeParams.map(tp=>tpp.parseParam(tp.name)).foreach(tp=>clas.typeParams += tp)
        clas.save        
    }
  }
  
  def createRelationships(entityToString: String, parents: List[model.TemplateEntity]) = {
    var clas = Class.find(By(Class.entityToString, entityToString)) openOr (error("Could not find " + entityToString))
    for (p <- parents) p match {
      case p: model.Package => {
        val par = Class.find(By(Class.entityToString, p.toString)).openOr(error("Could not find package" + p.toString)).memberClasses += clas
        par.save
      }
      case c: model.Class => clas.parents += Class.find(By(Class.entityToString, c.toString)).openOr(error("Could not find class" + c.toString))
      case o: model.Object => clas.parents += Class.find(By(Class.entityToString, o.toString)).openOr(error("Could not find object" + o.toString))
      case t: model.Trait => clas.parents += Class.find(By(Class.entityToString, t.toString)).openOr(error("Could not find trait" + t.toString))
      case _ => ()
    }
    clas.save
  }
  
  def createRootPackage(asString: String) = {
    Class.find(By(Class.entityToString, asString)) openOr 
      Class.create.entityToString(asString).name(asString).tlt(TopLevelType.Package).saveMe
    }
    
  def createPackage(pack: model.Package): Class = {
    Class.find(By(Class.entityToString, pack.toString)) openOr 
      Class.create.
        entityToString(pack.toString).
        name(pack.name).
        tlt(TopLevelType.Package).
        in(
          Class.find(By(Class.entityToString, pack.inTemplate.toString)).openOr(error("Couldn't find " + in.toString))
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
  object member extends MappedLongForeignKey(this, Member)
  
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
  object typeVar extends MappedString(this, 100)             //typeVar
  object res extends MappedLongForeignKey(this, Type)     //method or function   
  object funcArgs extends MappedOneToMany(Arg, Arg.typ, OrderBy(Arg.id, Ascending)) //list of args
  
  object methodArgs extends MappedOneToMany(Type, Type.typeFK, OrderBy(Type.id, Ascending)) //Methods (list of funcArgs)   
  
  object elements extends MappedOneToMany(Type, Type.typeFK, OrderBy(Type.id, Ascending)) //tuples
  object typeFK extends MappedLongForeignKey(this, Type) //foreign key for self referencing   
  object typeParam extends MappedOneToMany(TypeParam, TypeParam.typ, OrderBy(TypeParam.id, Ascending))  //Instance of 
  
  object concreteType extends MappedLongForeignKey(this, Class) 
  //object traits extends MappedOneToMany(Class, Class.typ, OrderBy(Class.id, Ascending))
  
  
  //TODO validation
  def isTuple: Boolean = 
    res.obj.isEmpty &&
    funcArgs.length == 0 &&
    methodArgs.length == 0 &&
    elements.length > 0 &&
    typeParam.length == 0
  def isFunc = ""
  def isMethod = ""
  def isTypeVar = ""
  def isInstanceOf = ""
}

object Type extends Type with LongKeyedMetaMapper[Type] {
  def createConcreteType(name: String) = {
    //Type.find(By(Type.concreteType, name)) openOr 
      Type.create.typeVar(name).typeType(TypeType.ConcreteType).saveMe    
  }
  
  def createTypeVar(name:String) = {
    Type.find(By(Type.typeVar, name)) openOr 
      Type.create.typeVar(name).typeType(TypeType.TypeVar).saveMe  
  }
  
  def createFunction(args: List[List[Type]], res: Type) = {
    var func = Type.create.res(res)
    //args foreach (_ foreach (x => func.funcArgs += x))
    func.saveMe
  }
  
  def createMethod(args: List[List[Type]], res: Type) = {
    Type.create.res(res).saveMe 
  }
  
  def createTuple(elems: List[Type]) = {
    var tuple = Type.create
    elems foreach (x => tuple.elements += x)
    tuple.saveMe
  }
}


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
  object order extends MappedInt(this)
}

object Arg extends Arg with LongKeyedMetaMapper[Arg] {
  def createArg(name: String, typ: Type, member: Member) {
    Arg.create.name(name).
    //typ().
    save
  }

}

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
  def createKind(tps: List[TypeParam]): Kind =
    tps match {
      case Nil     => nullary
      case p :: ps => arrow(p.kind.obj.open_!, createKind(ps))
    }
}

//Member
class Member extends LongKeyedMapper[Member] with IdPK with OneToMany[Long, Member]{
  def getSingleton = Member
  object entityToString extends MappedString(this, 200)
  object name extends MappedString(this, 100)
  object in extends MappedLongForeignKey(this, Class)
  object typeParams extends MappedOneToMany(TypeParam, TypeParam.member, OrderBy(TypeParam.id, Ascending))
  object memType extends MappedEnum[Member,MemType.type](this, MemType)
  object resultType extends MappedLongForeignKey(this, Type)
  object args extends MappedOneToMany(Arg, Arg.member, OrderBy(Arg.id, Ascending))
  
}

object Member extends Member with LongKeyedMetaMapper[Member] {
  //TODO Need to create types before this can finish
  //FIXME
  def constructMemberTypes(entity: model.Entity) = {
    def extractTypeVars(d: model.Entity): List[String] = {
      val tvs = for (entity <- d.toRoot.reverse) yield entity match {
        case dte: model.DocTemplateEntity => dte.typeParams map (_.name)
        case fn: model.Def => fn.typeParams map (_.name)
        case _ => List()
      }
      tvs.flatten
    }
    
    def parseResultType(d: model.Entity{val resultType: model.TypeEntity}): Option[Type] = {
      val parser = new TypeParser(new ScalaDocTypeLexer(extractTypeVars(d)))
      parser.parse(d.resultType.name.replace("\u21d2","=>")) match {
        case parser.Success(t, _) => Some(t)
        case f@parser.Failure(msg, pos) => {println(f);None}
        case _ => None
      }
    }
    
    def parseMethodArg(d: model.Entity{val resultType: model.TypeEntity}, inMethod: model.Entity): Option[Type] = {
      val methTVars = extractTypeVars(inMethod)
      val parser = new TypeParser(new ScalaDocTypeLexer(methTVars))
      parser.parse(d.resultType.name.replace("\u21d2","=>"), parser.funcParam) match {
        case parser.Success(t, _) => Some(t)
        case f@parser.Failure(msg, pos) => {println(f);None}
        case _ => None
      }
    }

    val t = entity match {
      case v: model.Val => {
        //Every val has an implicit "this" argument for the instance it is called on
        val thisArg: Option[Type] = parseResultType(v.inTemplate)
        Tuple2(thisArg, parseResultType(v))
      }
      case d: model.Def => {
        val resType = parseResultType(d)
        val argTypes = 
          for (argList <- d.valueParams) yield
            for (arg <- argList if !arg.isImplicit)
              yield parseMethodArg(arg, d).get

        //Every method has an implicit "this" argument for the instance it is called on
        val thisArg: Option[Type] = parseResultType(d.inTemplate)
        Tuple2(List(thisArg)::argTypes, resType)
      }
      case _ => None
    }
    t
  }
  def createMember(member: model.MemberEntity) = {
    
    val typeArgs, typeRes = constructMemberTypes(member)
    println("Args: " + typeArgs + "\tResult: " + typeRes)
    if (!(member.inheritedFrom.map(_.toString) exists (x => x == "scala.AnyRef" || x == "scala.Any"))) {
      val me = member match {
        case v: model.Val => v
        case d: model.Def => d
        case _ => error("Got entity in create Member that isn't a Def or Val'")
      }
      val mem = Member.create.
        entityToString(me.toString).
        name(me.name).
        in(Class.find(By(Class.entityToString, me.inTemplate.toString)).openOr
          (error("Could not find class: " + me.inTemplate.toString))).
        //Vals, Vars and Lazy Vals all use the trait model.Val
        memType(if (me.isDef) MemType.Def
                else if (me.isVal) MemType.Val
                else if (me.isVar) MemType.Var
                else if (me.isLazyVal) MemType.LazyVal
                else null
      ).saveMe
      if (member.isDef) {
        //typeParams
        val tpp = new TPParser
        val m = member.asInstanceOf[model.Def]
        m.typeParams.map(tp=>tpp.parseParam(tp.name)).foreach(tp=>mem.typeParams += tp)
        //args
        }
      mem.saveMe      
    }    
  }
}

