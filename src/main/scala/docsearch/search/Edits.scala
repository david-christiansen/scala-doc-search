package docsearch.search

import docsearch.query._

object Edits {
  //(A => Traversable[(A, Double)])
  def isOption(t: QType) = t match {
    case QTApp(QTName("Option"), _) => true
    case _ => false
  }

  val addOptionArg = (q: Query) => {
    def injectOption[T](t: T) = {
      for (argList <- q.args) yield
        argList map(x=> if (x == t && (!isOption(x.typ))) {QArg(x.name, QTApp(QTName("Option"), List(x.typ)))} else x)
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
    for (r <- res.flatten) yield (r, 0.1)
  }


  val addOptionResult = (q: Query) => {
    val res = Query(q.path,
                    q.memType,
                    q.name,
                    q.args,
                    if(isOption(q.resultType)) {
                      q.resultType
                    } else QTApp(QTName("Option"), List(q.resultType))
                    )
    List((res,0.2))
  }

}
