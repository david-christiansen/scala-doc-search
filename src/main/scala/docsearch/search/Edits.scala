package docsearch.search

import docsearch.query._
import docsearch.types.{Class, Inheritance, Type}

import net.liftweb.mapper._

trait ReCurryEdit {
  //Enumerate the way of splitting n up into sequences of numbers that add to n
  private def splits(n: Int): List[List[Int]] = {
    require(n >= 0);
    if (n == 0) Nil
    else {
      List(List(n)) ++ (for (m <- 1 to (n-1) toList; split <- splits(m)) yield (n-m) :: split)
    }
  }

  private def splitList[A](list: List[A]): List[List[List[A]]] = {
    def oneSplit(input: List[A], splitSpec: List[Int]): List[List[A]] = {
      require(input.length == splitSpec.foldLeft(0)(_+_))
      splitSpec match {
        case n :: ns => input.take(n) :: oneSplit(input.drop(n), ns)
        case Nil => Nil
      }
    }
    val splitSpecs = splits(list.length)
    for (spec <- splitSpecs) yield oneSplit(list, spec)
  }

  val reCurryCost = 0.7 //Cost parameter of recurrying

  val reCurry = (q: Query) => {
    q.args map {
      argLists: List[List[QArg]] =>
      val flatArgs = argLists.flatten
      for (currying <- splitList(flatArgs)) yield (q.copy(args=Some(currying)), reCurryCost)
    } match {
      case Some(neighbors) => neighbors
      case None => Nil
    }
  }
}

trait AddOptionEdits {
  private def isOption(t: QType) = {
    t match {
      case QTApp(QTName("Option"),_) => true
      case _ => false
    }
  }

  private def wrapOption(t: QType) = QTApp(QTName("Option"), List(t))

  val addOptionArg = (q: Query) => {
    def injectOption(t: QArg) = {
      for (argList <- q.args getOrElse List()) yield
        argList map(x=> if (x == t && (!isOption(x.typ))) {QArg(x.name, QTApp(QTName("Option"), List(x.typ)))} else x)
    }
    val res = for (argList <- q.args getOrElse List()) yield
                for (arg <- argList)
                  yield q.copy(args = Some(injectOption(arg)))
    for (r <- res.flatten) yield (r, 0.2)
  }

  val addOptionResult = (q: Query) => {
    if (isOption(q.resultType)) Nil
    else (q.copy(resultType=wrapOption(q.resultType)), 0.3) :: Nil
  }

}

trait ArgOrderEdit {

   //This permute method is taken from
   //http://scala-forum.org/read.php?4,330,2410#msg-2410

  private def permute[T](list: List[T]): List[List[T]] = {
    def removeFirst[T](elt: T, list: List[T]) : List[T] = list match {
      case Nil => Nil
      case x :: rest if (x == elt) => rest
      case other :: rest => other :: removeFirst(elt, rest)
    }
    list match {
      case Nil => List(Nil)
      case _ => for {
        elt <- list
        rest <- permute(removeFirst(elt,list))
      } yield elt::rest
    }
  }

  private def permuteSubLists[A](lists: List[List[A]]): List[List[List[A]]] =
    lists match {
      case Nil => Nil
      case l :: Nil => permute(l) map (List(_))
      case l :: ls => for (perm <- permute(l); rest <- permuteSubLists(ls)) yield perm::rest
    }

  val newArgOrder = (q: Query) => {
    q.args match {
      case Some(argLists) => for (newArgs <- permuteSubLists(argLists)) yield (q.copy(args=Some(newArgs)), 0.5)
      case None => Nil
    }
  }
}

trait superTypeEdit {
  val resultSuperType = (q: Query) => {
    val clas = Class.findAll(By(Class.name, q.resultType.toString))
    for (c <- clas; p <- c.parents if p.name != "AnyRef" ) yield (q.copy(resultType = p.toQType), 1.0)
  }
}

object QParser extends QueryParser

object Edits extends ReCurryEdit with AddOptionEdits with ArgOrderEdit with superTypeEdit{
  lazy val defaultEdits = List(addOptionArg, addOptionResult, reCurry, newArgOrder, resultSuperType)
}

object TestEdits extends Application {
  import bootstrap.liftweb.Boot
  val boot = (new Boot)
  boot.boot
  def test():Unit = {
    print("------Query> ")
    val input = Console.readLine()
    if (input != "q") {
      try{
        val r = Searcher.ParseQ.parseQ(input)
        val ss = new SearchState(r, Edits.defaultEdits:_*)
        ss.results.take(10) foreach(x=>println(x.getOrElse("No More Queries").toString))
      } catch {
        case _ => println("Error parsing")
      }
      test()
    }
  }

  test()
}
