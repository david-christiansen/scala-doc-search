package docsearch.search.edits
import docsearch.search._
import docsearch.types._
import docsearch.query.Query
import net.liftweb.mapper._

class Edits {
  //(A => Traversable[(A, Double)])
  val Option = (q: Query) => {
    val option = Class.find(By(Class.name, "Option"))
    val res = Type.find(By(Type.concreteType, option)).toList
    for (r <- res) yield (r, 1.0)
  }
  
  val matchExact = (q: Query) => {
    val res = q.name match {
      case Some(n) => Member.findAll(By(Member.name, n))
      case None => List()
    }
    for (r <- res) yield (r,1.0)
  }
  
  val matchExactName = (name: String) => {
    val res = Member.findAll(By(Member.name, name))
    for (r <- res) yield (r.toString, 1.0)
    //r: Traversable[(String, Double)]
  }
  
  val search = new SearchState("start", matchExactName)
}
