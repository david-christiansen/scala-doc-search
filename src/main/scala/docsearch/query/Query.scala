package docsearch.query

import docsearch.types.MemType._
import docsearch.types.TypeType
import docsearch.types.{Member, Type, Class, Arg}

import net.liftweb.mapper._


sealed abstract class QType {
  def query(): List[Type]
}

case class QTuple(elems: List[QType]) extends QType {
  def query = {
    val elemParents: List[Set[Long]] = elems.zipWithIndex map {
      case (qt, i) => qt.query.filter(_.elemOrder.is == i).map(_.typeFK.is).toSet
    }
    val hasAllElems = elemParents.tail.foldLeft(elemParents.head)(_ intersect _).toList
    Type.findAll(ByList(Type.id, hasAllElems), By(Type.numElems, elems.length), By(Type.typeType, TypeType.Tuple))
  }
}
case class QFunc(args: List[QType], res: QType) extends QType {
  def query() = {
    val argParentIds = args.zipWithIndex map {
      case (qt, i) =>
        val typIds = qt.query.map(_.id.is).toSet
        Arg.findAll(By(Arg.order, i)).filter(typIds contains _.typ.is).map(_.in.is).toSet
    }
    println(argParentIds)
    val hasAllArgs = argParentIds.foldLeft(argParentIds.head)(_ intersect _)
    println(hasAllArgs)

    val resTypeIds = res.query.map(_.id.is) toSet

    Type.findAll(By(Type.typeType, TypeType.Function)).
      filter(hasAllArgs contains _.id.is).
      filter(resTypeIds contains _.res.is)
  }
}

case class QTVar(name: String) extends QType {
  //FIXME: Worry about consistent assignments of vars
  def query() = Type.findAll(By(Type.typeType, TypeType.TypeVar))
}

case class QTName(name: String) extends QType {
  def query() = {
    Type.findAll(By(Type.typeType, TypeType.ConcreteDummy), Like(Type.name, "%."+name)) ++
    Type.findAll(By(Type.typeType, TypeType.ConcreteDummy), By(Type.name, name)) ++
    Type.findAll(By(Type.typeType, TypeType.ConcreteType), In(Type.concreteType, Class.id, By(Class.name, name)))
  }
}

case class QTApp(op: QType, args: List[QType]) extends QType {
  def query() = {
    val opT = op.query
    val argApps = args.zipWithIndex map {
      case (arg, i) => arg.query.filter(_.typeArgOrder == i).map(_.typeFK.is).toSet
    }
    val hasAllArgs = argApps.tail.foldLeft(argApps.head)(_ intersect _)

    Type.findAll(
      By(Type.typeType, TypeType.TypeApp),
      ByList(Type.appOp, opT.map(_.id.is))
    ).filter(hasAllArgs contains _.id.is)
  }
}

case object QTWildcard extends QType {
  def query() = Type.findAll
}

case class Query( path: Option[QPath],
                  memType: Option[MemType],
                  name: Option[String],
                  args: Option[List[List[QArg]]],
                  resultType: QType) {
  override def toString = {
    "Query(path= " + path.toString +
    ", memtype= " + memType.toString +
    ", name= " + name.toString +
    ", args= " + args.toString +
    ", resulttype= " + resultType + ")"
  }
  def findMatching(): List[Member] = {
    println("Finding matching for " + this)
    val queryParams =
      (memType map (By(Member.memType, _)) toList) ++
      (name map (By(Member.name, _)) toList) ++
      List(By(Member.argCounts, args.map(_.length).mkString(",")))

    val validTypes: Set[Long] = resultType.query() map (_.id.is) toSet

    val memberResults = Member.findAll(queryParams:_*) filter { mem: Member =>
      validTypes.contains(mem.resultType.is)
    }

    args match {
      case Some(argLists) => {
        val argTypes: List[List[Set[Long]]] =
          for ((argList, outer) <- argLists.zipWithIndex)
          yield for ((arg, inner) <- argList.zipWithIndex)
                yield arg.query(outer, inner).map(_.member.is).toSet

        val memberFromArgLists: List[Set[Long]] = argTypes map { argList =>
          if (argList.length > 0)
            argList.tail.foldLeft(argList.head)(_ intersect _)
          else Set.empty[Long]
        }
        val memberFromArgs: Set[Long] =
          if (memberFromArgLists.length > 0)
            memberFromArgLists.tail.foldLeft(memberFromArgLists.head)(_++_)
          else Set.empty[Long]
        memberResults.filter(memberFromArgs contains _.id.is)
      }
      case None => memberResults
    }
  }
}

case class QPath(components: List[String]) {
  override def toString = components.mkString(".")
}

case class QArg(name: Option[String], typ: QType) {
  override def toString = name match {
    case Some(n) => n + ": " + typ.toString
    case None => typ.toString
  }

  def query(outer: Int, inner: Int) = {
    val types = typ.query.map(_.id.is).toSet
    val params = List(
      By(Arg.listOrder, outer),
      By(Arg.order, inner)
    ) ++ name.map(
      By(Arg.name, _)
    ).toList

    Arg.findAll(params:_*).filter(types contains _.typ.is)
  }
}

object TestQueries extends QueryParser with Application {
  import bootstrap.liftweb.Boot
  val boot = (new Boot)
  boot.boot
  def test():Unit = {
    print("------Query> ")
    val input = Console.readLine()
    if (input != "q") {
      val q = parse(input, query)
      q match {
        case Success(p,_) => {
          p.asInstanceOf[Query].findMatching() foreach(x=>println("RESULT: " + x.in.obj.open_!.toString + "#" + x.name))
        }
        case _ => println("Failed to parse")
      }
      test()
    }
  }

  test()
}


