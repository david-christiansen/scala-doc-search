package docsearch.search

import docsearch.query.{Query, QType, QArg}

object Edits {
  //(A => Traversable[(A, Double)])
  def isOption(t: QType) = {
    if(t.str == "Option") true
    else false               
  }   
    
  val addOptionArg = (q: Query) => {
    def injectOption[T](t: T) = {
      for (argList <- q.args) yield
        argList map(x=> if (x == t && (!isOption(x.typ))) {QArg(x.name, QType("Option", List(x.typ)))} else x)
    }
    //this is ugly and only works for Lists. My head exploded trying to get this to work for List[List[A]] 
    //There's probaby a more general and elegant solution that I can't see yet
    //((for (i <- d) yield d) zipWithIndex) map(k=>k._1.map(x=>if (d(k._2)==x) {Some(x)} else x))
    //FIXME this can potentially add more than one Option at a time
    val res = for (argList <- q.args) yield
                for (arg <- argList)
                  yield Query(q.path, 
                          q.memType, 
                          q.name, 
                          injectOption(arg),
                          q.resultType
                          )
    for (r <- res.flatten) yield (r, 0.2)
  }
  
  
  val addOptionResult = (q: Query) => {
    val res = Query(q.path, 
                    q.memType, 
                    q.name, 
                    q.args, 
                    if(isOption(q.resultType)) { 
                      q.resultType
                    } else QType("Option", List(q.resultType))
                    )
    List((res,0.3))
  }
  
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
  

}
