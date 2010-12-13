package docsearch.search

import docsearch.query._

object Edits {
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
/*  
  val curry = (q: Query) => {
  //FIXME Only curries the first curriable argument starting from the left
    def createArgs[T](argsList: List[List[T]]): List[List[T]] = 
      argsList match {
        case h :: Nil => {
          if (h.size > 1) List(List(h.head) , h.tail)
          else h :: Nil
        }
        case h :: t => {
          if (h.size > 1) List(List(h.head) , h.tail) ++ t
          else h :: createArgs(t)
        }
        case Nil => Nil
      }
    val res = Query(q.path, 
                    q.memType, 
                    q.name, 
                    createArgs(q.args),
                    q.resultType
                    )
    List((res,0.4))
  }
  
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
