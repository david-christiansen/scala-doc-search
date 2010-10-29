package docsearch.dumper

import scala.tools.nsc.doc.{model, Universe}
import scala.collection.mutable

import docsearch.types._

object DataDumper {
  def generate(docModel: Universe) = {
    val rootPackage = docModel.rootPackage
    
    val seen: mutable.Map[model.Entity, Option[Package]] = mutable.Map.empty
    val todo: mutable.Stack[model.Entity] = mutable.Stack(rootPackage)
    
    while (todo.length > 0) {
      val current = todo.pop()
      println("Processing " + current.toString)
      for (child <- contents(current)) {
        if (!seen.contains(child)) todo.push(child)
        seen += child -> convert(child, seen)
      }
    }
  }

  def contents(entity: model.Entity) =
    entity match {
      case dte: model.DocTemplateEntity => dte.members
      case _ => Seq.empty
    }

  def convert(entity: model.Entity, seen: mutable.Map[model.Entity, Option[Package]]): Option[Package] = {
    if (seen contains entity) {
      seen(entity)
    }
    else {
      entity match {
        case t: model.Trait => 
          Some(Class(
            t.name,
            convert(t.inTemplate, seen) match {
              case Some(p) => p
              case None => error("IN TRAIT: Could not convert parent: " + t.inTemplate.toString + "(" + t.inTemplate.getClass + ") of: "+ t.toString + "\n\nResult of convert: "+ convert(t.inTemplate, seen))
            },
            List(), //members
            List(), //inherits
            List(), //constructor
            t.typeParams.map(convertTypeParam),
            ClassOrTrait.Trait //Need to figure out if class or trait
          ))
        case x: model.RootPackage => Some(PathRoot)
        case p: model.Package =>
          Some(NamedPackage(
            convert(p.inTemplate, seen) match {
              case Some(p) => p.asInstanceOf[Package] /* FIXME */
              case None => error("IN PACKAGE: Could not convert parent: " + p.inTemplate.toString + " of " + p.toString)
            },
            p.name
          ))
        case o: model.Object =>
          Some(Object(
            convert(o.inTemplate, seen) match {
              case Some(o) => o
              case None => error("IN Object: Could not convert parent: " + o.inTemplate.toString + " of " + o.toString)
            },
            o.name,
            List()
          ))
        case _ => None
      }
    }
  }

  def convertTypeParam(t: model.TypeParam): (String, Kind) = {
    val parser = new TPParser
    parser.parseParam(t.name) match {
      case parser.Success(param: TypeParam, _) => (param.name, param.kind)
      case _ => error("could not parse type parameter '" + t.name + "'")
    }
  }
}

