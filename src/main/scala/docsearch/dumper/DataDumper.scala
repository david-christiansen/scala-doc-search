package docsearch.dumper

import scala.tools.nsc.doc.{model, Universe}
import scala.collection.mutable

import docsearch.types._

object DataDumper {
  import bootstrap.liftweb.Boot
  (new Boot).boot

  val seen: mutable.Set[model.Entity] = mutable.Set.empty


  def generate(docModel: Universe) = {
    val rootPackage = docModel.rootPackage
    
    val todo: mutable.Stack[model.Entity] = mutable.Stack(rootPackage)

    while (todo.length > 0) {
      val current = todo.pop()
      println("Processing " + current.toString)
      for (child <- contents(current)) {
        if (!seen.contains(child)) todo.push(child)
        seen += child 
      }
      convert(current)
    }
  }

  def contents(entity: model.Entity) =
    entity match {
      case dte: model.DocTemplateEntity => dte.members
      case _ => Seq.empty
    }

  def convert(entity: model.Entity): Unit = {
    println("Processing " + entity.toString)
    entity match {
      case t: model.Trait => {
        var newT = Class.create.name(t.name).saveMe
        for ((p, i) <- t.typeParams.zip(0 to t.typeParams.length)) {
          val converted = convertTypeParam(p)
          newT.typeParams += converted
          converted.save
          println(converted)
        }
      }
      case x: model.RootPackage =>
        Class.create.name("_root_").typ(TypeEnum.Package).save
      case p: model.Package =>
        Class.create.name(p.name).typ(TypeEnum.Package).save
      case o: model.Object =>
        Class.create.name(o.name).typ(TypeEnum.Object).save
      case _ => ()
    }
  }
  

  def convertTypeParam(t: model.TypeParam): TypeParam = {
    val parser = new TPParser
    parser.parseParam(t.name) match {
      case parser.Success(param: TypeParam, _) => param
      case _ => error("could not parse type parameter '" + t.name + "'")
    }
  }
}

