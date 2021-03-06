package docsearch.types

import docsearch.dumper.{TPParser, TypeParser, ScalaDocTypeLexer}
import docsearch.query._

import net.liftweb.mapper._
import net.liftweb.common.{Box,Full,Empty,Failure,ParamFailure}

import scala.tools.nsc.doc.{model, Universe}

import scala.xml._
import docsearch.utils._

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

  def topLevelFromTemplate(te: model.TemplateEntity) = {
    if (te.isTrait) TopLevelType.Trait
    else if (te.isObject) TopLevelType.Object
    else if (te.isPackage) TopLevelType.Package
    else TopLevelType.Class
  }

}

import TopLevelType._

object TypeType extends Enumeration {
  type TypeType = Value
  val Tuple = Value("tuple")
  val Function = Value("function")
  val Method = Value("method")
  val TypeVar = Value("type variable")
  val ConcreteType = Value("concrete type")
  val ConcreteDummy = Value("concrete type (name only)")
  val TypeApp = Value("type application")
  val Wildcard = Value("wildcard")
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

object Utils {
  def extractTypeVars(d: model.Entity): List[String] = {
    val tvs = for (entity <- d.toRoot.reverse) yield entity match {
      case dte: model.DocTemplateEntity => dte.typeParams map (_.name)
      case fn: model.Def => fn.typeParams map (_.name)
      case _ => List()
    }
    tvs.flatten
  }

  def extractTypeQualifiers(entity: model.TypeEntity): Map[String, String] = {
    (for ((start, (t, end)) <- entity.refEntity)
     yield (entity.name.substring(start, start+end) -> t.toString)).toMap
  }
}

import Utils._

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

  object parents extends MappedManyToMany(Inheritance, Inheritance.children, Inheritance.parents, Type)

  object typeParams extends MappedOneToMany(TypeParam, TypeParam.clas, OrderBy(TypeParam.order, Ascending))
  //TODO Add contructor to Classes
  object constructor extends MappedLongForeignKey(this, Arg)

  def path = this.in.obj.map(List(_)).openOr(List()) ++ List(this)

  override def toString() = {
    //FIXME add type params and such
    path.map(_.name.is).mkString(".")
  }

  private def whichTypeXhtml(): NodeSeq = this.tlt.is match {
    case TopLevelType.Package => Text("package")
    case TopLevelType.Object => Text("object")
    case TopLevelType.Class => Text("class")
    case TopLevelType.Trait => Text("trait")
    case _ => Text ("[]")
  }

  private def classNameXhtml: NodeSeq = {
    <span class="className" id={this.id.toString}>{this.name.is}</span> ++
    (if (this.typeParams.length > 0)
      <span class="typeParams">
        {(NodeSeq.Empty ++ this.typeParams.map {
          tp: TypeParam => <span class="typeParam" id={tp.id.toString}>{tp.name.is}</span>
         }).mkNodes(Text("["), Text(", "), Text("]"))
        }
      </span>
     else NodeSeq.Empty)
  }

  private def inheritsXhtml(): NodeSeq =
    if (this.parents.length > 0)
      Text(" extends ") ++ this.parents.head.toXhtml ++
      (NodeSeq.Empty ++ this.parents.tail.flatMap((c: Type) => Text(" with ") ++ this.classNameXhtml))
    else NodeSeq.Empty


  def toXhtml = whichTypeXhtml ++ Text(" ") ++ classNameXhtml ++ Text(" ") ++ inheritsXhtml
}

object Class extends Class with LongKeyedMetaMapper[Class] {

  //creates a class, trait or object
  def createClass(dte: model.DocTemplateEntity) = {
    val tpp = new TPParser
    val tlt = dte match {
      case c: model.Class => TopLevelType.Class
      case o: model.Object => TopLevelType.Object
      case t: model.Trait => TopLevelType.Trait
      case _ => error("Got something that wasn't a trait class or object in createClass'")
    }
    Class.find(By(Class.entityToString, dte.toString), By(Class.tlt, tlt)) openOr {
      val clas = Class.create.entityToString(dte.toString).
        name(dte.name).
        tlt(tlt).
        in(
          Class.find(By(Class.entityToString, dte.inTemplate.toString)).openOr(error("Could not find " + in))
        ).saveMe
        dte.typeParams.map(tp=>tpp.parseParam(tp.name)).foreach(tp=>clas.typeParams += tp)
        clas.save
    }
  }

  def findForTE(clas: model.TemplateEntity): Box[Class] = {
    Class.find(
      By(Class.entityToString, clas.toString),
      By(Class.tlt, TopLevelType.topLevelFromTemplate(clas))
    )
  }

  def addSupers(clas: model.DocTemplateEntity) = {
    val me = findForTE(clas).open_!

    clas.parentType foreach { te: model.TypeEntity =>
      val types = extractTypeQualifiers(te)
      val typeVars = extractTypeVars(clas)
      te.name.split(" with ") foreach { name: String =>
        val parser = new TypeParser(new ScalaDocTypeLexer(typeVars, types))
        parser.parse(name.replace("\u21d2","=>")) match {
          case parser.Success(t, _) => me.parents += t
          case _ => println("Couldn't parse parent type " + name)
        }
      }
    }

    //Note: Java superclasses don't get included.  This is a silly limitation that will one day be fixed.

    me.save
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
  object parents extends MappedLongForeignKey(this, Type)
}

object Inheritance extends Inheritance with MetaMapper[Inheritance]

//Type Param
class TypeParam extends LongKeyedMapper[TypeParam] with IdPK with OneToMany[Long, TypeParam] with ManyToMany {
  def getSingleton = TypeParam
  object name extends MappedString(this, 100)
  object order extends MappedInt(this)
  object kind extends MappedLongForeignKey(this, Kind)
  object params extends MappedOneToMany(TypeParam, TypeParam.parent, OrderBy(TypeParam.order, Ascending))
  object parent extends MappedLongForeignKey(this, TypeParam) // NULL if top-level
  object member extends MappedLongForeignKey(this, Member)
  object clas extends MappedLongForeignKey(this, Class)

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
  object args extends MappedOneToMany(Arg, Arg.in, OrderBy(Arg.order, Ascending)) //list of args

  object elements extends MappedOneToMany(Type, Type.typeFK, OrderBy(Type.id, Ascending)) //tuples
  object elemOrder extends MappedInt(this)
  object numElems extends MappedInt(this) // number of elements in tuple
  object typeFK extends MappedLongForeignKey(this, Type) //foreign key for self referencing
//  object typeParams extends MappedOneToMany(Type, Type.typeFK, OrderBy(Type.id, Ascending))  //Instance of

  object concreteType extends MappedLongForeignKey(this, Class)

  object name extends MappedString(this, 100)

  object traits extends MappedOneToMany(Class, Class.typ, OrderBy(Class.id, Ascending))

  object appOp extends MappedLongForeignKey(this, Type)
  object typeArgOrder extends MappedInt(this)
  object typeArgs extends MappedOneToMany(Type, Type.typeFK, OrderBy(Type.typeArgOrder, Ascending))

  def toXhtml: NodeSeq = {
    this.typeType match {
      case Tuple => {
        val contentNodes: NodeSeq = this.elements.flatMap(_.toXhtml)
        val contents = contentNodes.parenList
        <span class="typeTuple" id={this.id.toString}>{contents}</span>
      }
      case Function => {
        val argTypes: NodeSeq = this.args.flatMap(_.typ.obj.map(_.toXhtml).openOr(Text("ERRORTYPE")))
        <span class="function" id={this.id.toString}>
          {argTypes.parenList ++ <span class="funcArrow">{EntityRef("rArr")}</span> ++ this.res.obj.map(_.toXhtml).openOr(Text("ERRORTYPE"))}
        </span>
      }
      case TypeVar => <span class="typeVar" id={this.id.toString}>{this.typeVar.is}</span>
      case ConcreteType => <span class="concreteType" id={this.id.toString}>{this.concreteType.obj.map(_.toString).openOr("nope")}</span>
      case ConcreteDummy => <span class="concreteDummy" id={this.id.toString}>{this.name.is}</span>
      case TypeApp =>
        <span class="typeApp" id={this.id.toString}>
          <span class="typeAppOp">{this.appOp.obj.map(_.toXhtml).openOr(Text("NOT FOUND"))}</span>
          <span class="typeAppArgs">
            [{(NodeSeq.Empty ++ this.typeArgs.map((n: Type) => <span class="typeAppArg">{n.toXhtml}</span>)).mkNodes(Text(", "))}]
          </span>
        </span>
      case Wildcard => Text("_")
      case _ => <span class="notdone">Not done</span>
    }
  }

  override def toString = {
    this.typeType match {
      case Tuple => this.elements.map(_.toString).mkString("(", ", ", ")")
      case Function => {
        val argTypes = this.args.map(_.typ.obj.map(_.toString).openOr("ERRORTYPE"))
        argTypes.mkString("(", ", ", ")") + "=>" + this.res.obj.map(_.toString).openOr("ERRORTYPE")
      }
      case Method => this.args.map(_.toString).mkString("(", ", ", ")") + "method"
      case TypeVar => this.typeVar.is
      case ConcreteType => this.concreteType.obj.map(_.toString).openOr("nope")
      case ConcreteDummy => this.name.is
      case TypeApp => this.appOp.obj.openOr("NOT FOUND!!").toString + this.typeArgs.map(_.toString).mkString("[", ", ", "]")
      case Wildcard => "_"
    }
  }
  def toQType: QType= {
    this.typeType match {
      case Tuple => QTuple(this.elements.map(_.toQType).toList)
      case Function => {
        val argTypes = this.args.map(_.typ.obj.map(_.toQType).openOr(error("ERRORTYPE on method toQType"))).toList
        QFunc(argTypes, this.res.obj.map(_.toQType).openOr(error("ERRORTYPE on method toQType")))
      }
      //case Method => this.args.map(_.toString).mkString("(", ", ", ")") + "method"
      case TypeVar => QTVar(this.typeVar.is)
      case ConcreteType => QTName(this.concreteType.obj.map(_.toString).openOr(error("ERRORTYPE on method toQType in ConcreteType")))
      case ConcreteDummy => QTName(this.name.is)
      case TypeApp => QTApp(this.appOp.obj.openOr(error("ERRORTYPE on method toQType in TypeApp")).toQType, 
                            this.typeArgs.map(_.toQType).toList)
      case Wildcard => QTWildcard
    }
  }


  //TODO validation
  def isTuple: Boolean =
    res.obj.isEmpty &&
    args.length == 0 &&
    elements.length > 0 &&
    typeArgs.length == 0
  def isFunc = ""
  def isMethod = ""
  def isTypeVar = ""
  def isInstanceOf = ""
}

object Type extends Type with LongKeyedMetaMapper[Type] {
  def createConcreteType(name: String) = {
    val clas = Class.find(
      By(Class.entityToString, name)
    )
    val ct = clas match {
      case Full(c) => Type.create.concreteType(c).typeType(TypeType.ConcreteType).saveMe
      case _ => Type.create.name(name).typeType(TypeType.ConcreteDummy).saveMe
    }
    ct.saveMe
  }

  def createTypeApp(typ: Type, args: List[Type]) = {
    require(typ.typeType == TypeVar || typ.typeType == ConcreteType || typ.typeType == ConcreteDummy)

    val t = Type.create.typeType(TypeType.TypeApp).appOp(typ).saveMe
    for ((arg, i) <- args.zipWithIndex) {
      arg.save
      t.typeArgs += arg.typeArgOrder(i).saveMe
    }
    t.saveMe
   }

  def createTypeVar(name:String) = {
    val t = Type.create.typeVar(name).typeType(TypeType.TypeVar)
    t.saveMe
  }

  def addTraits(name: Type, traits: List[Type]) = {
    for (t <- traits if (t.typeType == TypeType.ConcreteType))
      name.traits += t.concreteType.obj.openOr(error("Could not retrieve class while adding traits: " + t))
    name.saveMe
  }

  def createFunction(args: List[Type], res: Type) = {
    val f = Type.create.typeType(TypeType.Function).res(res).saveMe
    for ((arg, i) <- args.zipWithIndex) {
      f.args += Arg.create.name("anonymous").typ(arg).order(i).saveMe
    }

    f.saveMe
  }

  def createTuple(elems: List[Type]) = {
    require(elems.length >= 1) // All tuples have at least 1 element
    var tuple = Type.create.typeType(TypeType.Tuple).numElems(elems.length)
    for ((elem, i) <- elems.zipWithIndex) {
      elem.save
      tuple.elements += elem.elemOrder(i).saveMe
    }
    tuple.save
    assert(tuple.elements.length > 0)
    tuple
  }

  def wildcard() = {
    Type.create.typeType(Wildcard).saveMe
  }
}

//Arg, Anonymous methods have no member and name anonymous
class Arg extends LongKeyedMapper[Arg] with IdPK with ManyToMany {
  def getSingleton = Arg
  object name extends MappedString(this, 100)
  object typ extends MappedLongForeignKey(this, Type)
  object in extends MappedLongForeignKey(this, Type)
  object member extends MappedLongForeignKey(this, Member)
  object order extends MappedInt(this)
  object listOrder extends MappedInt(this)
  object isImplicit extends MappedBoolean(this)

  override def toString() = name + " : " + typ.obj.toString

  def toXhtml(): NodeSeq =
      <span class="arg">
        {if (this.isImplicit) Text("Implicit") else NodeSeq.Empty}
        {Text(this.name.is)}:
        {this.typ.map(_.toXhtml).openOr(Text("ERROR"))}
      </span>

}

object Arg extends Arg with LongKeyedMetaMapper[Arg] {
  def createArg(name: String, typ: Type, member: Member, order: Int) {
    Arg.create.name(name).typ(typ).order(order).saveMe
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
class Member extends LongKeyedMapper[Member] with IdPK with OneToMany[Long, Member] {

  def getSingleton = Member
  object entityToString extends MappedString(this, 200)
  object name extends MappedString(this, 100)
  object in extends MappedLongForeignKey(this, Class)
  object typeParams extends MappedOneToMany(TypeParam, TypeParam.member, OrderBy(TypeParam.order, Ascending))
  object memType extends MappedEnum[Member,MemType.type](this, MemType)
  object resultType extends MappedLongForeignKey(this, Type)
  object args extends MappedOneToMany(Arg, Arg.member, OrderBy(Arg.listOrder, Ascending))
  object argCounts extends MappedString(this, 50)

  def getArgLists(): List[List[Arg]] = {
    // lists gets the argument lists in the correct order, split from one big list
    val lists: List[List[Arg]] =
      args.groupBy(_.listOrder.is).toList.sortWith(_._1 < _._1).map(_._2.toList)
    //Now sort the individual argument lists
    lists map {l: List[Arg] => l.sortWith(_.order.is < _.order.is)}
  }

  def flattenNodes(ns: Seq[NodeSeq]): NodeSeq =
      ns.foldLeft(NodeSeq.Empty) {(a: NodeSeq, b: NodeSeq) => a ++ b}

  def toXhtml(): NodeSeq = {
    val memType = this.memType.toString
    val name = Text(this.name.is)
    val args: NodeSeq = {
      val aLists = getArgLists flatMap {
        argList => <span class="argList">{flattenNodes(argList.map(_.toXhtml)).parenList}</span>
      }
      <span class="argLists">{flattenNodes(aLists)}</span>
    }
    val resType = resultType.obj.map(_.toXhtml).openOr("NO TYPE!")
    val typeParams = {
      if (this.typeParams.length == 0) NodeSeq.Empty
      else {
        val params: NodeSeq = this.typeParams map {
          p: TypeParam => <span class="typeParam" id={p.id.toString}>{p.name.is}</span>
        }
        <span class="typeParams">{params.mkNodes(Text("["), Text(", "), Text("]"))}</span>
      }
    }

    <span class="member">
      {Text(memType)}
      {Text(" ")}
      <span class="memName">{name}</span>
      {typeParams}
      {args}:  {resType}
    </span>
  }
}

object Member extends Member with LongKeyedMetaMapper[Member] {
  def constructMemberTypes(entity: model.Entity, mem: Member) = {
    def parseResultType(d: model.Entity{val resultType: model.TypeEntity}): Option[Type] = {
      val parser = new TypeParser(new ScalaDocTypeLexer(
        extractTypeVars(d),
        extractTypeQualifiers(d.resultType)
      ))
      parser.parse(d.resultType.name.replace("\u21d2","=>")) match {
        case parser.Success(t, _) => Some(t)
        case f@parser.Failure(msg, pos) => {println(f);None}
        case _ => None
      }
    }

    def parseMethodArg(d: model.Entity{val resultType: model.TypeEntity}, inMethod: model.Entity): Option[Type] = {
      val methTVars = extractTypeVars(inMethod)
      val methTQual = extractTypeQualifiers(d.resultType)
      val parser = new TypeParser(new ScalaDocTypeLexer(methTVars, methTQual))
      parser.parse(d.resultType.name.replace("\u21d2","=>"), parser.funcParam) match {
        case parser.Success(t, _) => Some(t)
        case f@parser.Failure(msg, pos) => {println(f);None}
        case _ => None
      }
    }

    val t = entity match {
      case v: model.Val => {
        mem.resultType(parseResultType(v).get).save
      }
      case d: model.Def => {
        var outer, inner = 0
        for (argList <- d.valueParams) {
          inner = 0
          for (arg <- argList){
            val argType = parseMethodArg(arg, d).get
            mem.args += Arg.create.name(arg.name).
                            typ(argType).
                            isImplicit(arg.isImplicit).
                            order(inner).
                            listOrder(outer).saveMe
            inner += 1
          }
          outer += 1
        }
        mem.resultType(parseResultType(d).get).argCounts(d.valueParams.map(_.length).mkString(",")).save
      }
      case _ => None
    }
  }

  def createMember(member: model.MemberEntity) = {
    if (!(member.inheritedFrom.map(_.toString) exists (x => x == "scala.AnyRef" || x == "scala.Any"))) {
      val containerType = TopLevelType.topLevelFromTemplate(member.inTemplate)

      val mem = Member.create.
        entityToString(member.toString).
        name(member.name).
        in(Class.find(By(Class.entityToString, member.inTemplate.toString), By(Class.tlt, containerType)).openOr
          (error("Could not find class: " + member.inTemplate.toString))).
        //Vals, Vars and Lazy Vals all use the trait model.Val
        memType(if (member.isDef) MemType.Def
                else if (member.isVal) MemType.Val
                else if (member.isVar) MemType.Var
                else if (member.isLazyVal) MemType.LazyVal
                else null
      ).saveMe
      if (member.isDef) {
        //typeParams
        val tpp = new TPParser
        val m = member.asInstanceOf[model.Def]
        m.typeParams.map(tp=>tpp.parseParam(tp.name)).foreach(tp=>mem.typeParams += tp)
        }
      mem.save
      constructMemberTypes(member, mem)
    }
  }
}

