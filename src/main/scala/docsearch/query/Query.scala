package docsearch.query

import docsearch.types.MemType._
import docsearch.types.TypeType
import docsearch.types.{Member, Type, Class}

import net.liftweb.mapper._

sealed abstract class QType {
//def query(): List[(Type.Somefield, Type.Somefield Type)] I think this is what I really want   MappedField
  def query(): List[Type]//List[QueryParam] //I think is much less efficient
}

case class QTuple(elems: List[QType]) extends QType {
  //def query() = elems map((TypeType.Tuple, _.query())
  val queryParams = elems flatMap(x => x.query())
  
  def query = Type.findAll(By(Type.typeType, TypeType.Tuple))
                           //By(Type.elements, queryParams))
}
case class QFunc(args: List[QType], res: QType) extends QType {
  def query() = List()
}

case class QTVar(name: String) extends QType {
  def query() = {
    Type.findAll(By(Type.typeType, TypeType.TypeVar),
                 By(Type.typeVar, name))
  }
}

case class QTName(name: String) extends QType {
  def query() = {
    Type.findAll(By(Type.typeType, TypeType.ConcreteType),
                 In(Type.concreteType, Class.id, By(Class.name, name)))
  }
}

case class QTApp(op: QType, args: List[QType]) extends QType {
  def query() = List()
}

case class Query( path: Option[QPath],
                  memType: Option[MemType],
                  name: Option[String],
                  args: List[List[QArg]],
                  resultType: QType) {
  override def toString = {
    "Query: path: " + path.toString +
    "\n\tmem type: " + memType.toString +
    "\n\tname: " + name.toString +
    "\n\targs: " + args.toString +
    "\n\tresult type: " + resultType
  }
  def findMatching(): List[Member] = {
    val queryParams =
      //List(PreCache(Member.in)) ++
      //FIXME this path is wrong
      /* List(Like(Member.entityToString, path.toString + "#%")) ++ */
      (memType map (By(Member.memType, _)) toList) ++
      (name map (By(Member.name, _)) toList) //++
      //(args) ++
      //(resultType.query() map (By(Member.resultType, _)) toList)
    //Member.findAll(By(Member.resultType, By(Type.typeVar, "A")))
    Member.findAll(queryParams:_*)
  }
}

case class QPath(components: List[String]){
  override def toString = components.mkString(".")
}

case class QArg(name: Option[String], typ: QType){
  override def toString = name match {
    case Some(n) => n + ": " + typ.toString
    case None => typ.toString
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


