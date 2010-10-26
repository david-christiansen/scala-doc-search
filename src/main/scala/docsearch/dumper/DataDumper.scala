package docsearch.dumper

import scala.tools.nsc.doc.{model, Universe}
import scala.collection.mutable

object DataDumper {
  def generate(docModel: Universe) = {
    val rootPackage = docModel.rootPackage
    
    val seen: mutable.Set[model.Entity] = mutable.Set.empty
    val todo: mutable.Stack[model.Entity] = mutable.Stack(rootPackage)
    
    while (todo.length > 0) {
      val current = todo.pop()
      println("Processing " + current.toString)
      for (child <- contents(current)) {
        if (!seen.contains(child)) todo.push(child)
        seen += child
      }
    }
  }

  def contents(entity: model.Entity) =
    entity match {
      case dte: model.DocTemplateEntity => dte.members
      case _ => Seq.empty
    }
}
