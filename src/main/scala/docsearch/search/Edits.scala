package docsearch.search

import docsearch.query._

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
  def isOption(t: QType) = {
    t match {
      case QTApp(QTName("Option"),_) => true
      case _ => false
    }
  }

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
    val res = q.copy(resultType = {
      if(isOption(q.resultType)) {
        q.resultType
      } else QTApp(QTName("Option"), List(q.resultType))
    })
    List((res,0.3))
  }

}

object Edits extends ReCurryEdit with AddOptionEdits {
  lazy val defaultEdits = List(addOptionArg, addOptionResult, reCurry)

/*  

  val argOrder = (q: Query) => {
    //TODO Reference: Permute method taken from http://scala-forum.org/read.php?4,330,2410#msg-2410
    //FIXME only works on non curried functions because of how this algorithm reduces things down
    def permute[T](liste: List[T]): List[List[T]] = {     
      def retire[T](elt: T, liste: List[T]) : List[T] = liste match {
        case Nil => Nil
        case x::reste if (x == elt) => reste
        case other::reste => other::retire(elt, reste)
      }
     
      liste match {
        case Nil => List(Nil)
        case _ => for { 
                  elt <- liste
                  reste <- permute(retire(elt,liste))
                } yield elt::reste
      }
    }
    if (q.args.size > 1 || q.args.size < 1) List()
    else {
      val argsLists = permute(q.args.head)
      val res = for (argList <- argsLists) yield
        Query(q.path, 
          q.memType, 
          q.name, 
          List(argList), 
          q.resultType
          )
        for (r <- res) yield (r, 0.1)
    }
  }
*/ 
}

object TestEdits extends Application {
  def test():Unit = {
    print("------Query> ")
    val input = Console.readLine()
    if (input != "q") {
      try{
        val r = Searcher.ParseQ.parseQ(input)
        val ss = new SearchState(r, Edits.addOptionArg)
        ss.results.take(10) foreach println
      } catch {
        case _ => println("Error parsing")
      }
      test()
    }
  }

  test()
}
