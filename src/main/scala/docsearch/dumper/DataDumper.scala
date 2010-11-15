package docsearch.dumper

import scala.tools.nsc.doc.{model, Universe}
import scala.collection.mutable

import docsearch.types._

object DataDumper {
  import bootstrap.liftweb.Boot
  val boot = (new Boot)
  boot.boot
  boot.clearDB
  boot.createDB

  val seen: mutable.Set[model.Entity] = mutable.Set.empty


  def generate(docModel: Universe) = {
    val rootPackage = docModel.rootPackage
    
    val todo: mutable.Queue[model.Entity] = mutable.Queue(rootPackage)

    while (todo.length > 0) {
      val current = todo.dequeue()
      convert(current)

      for (child <- contents(current)) {
        if (!seen.contains(child) &&  //Horrid hack for preventing wierd orphan behavior:
            (current.toString == "_root_" || child.toString.startsWith(current.toString))) 
          todo += child
        seen += child
      }
      
    }
  }

  def contents(entity: model.Entity) = {
    val children = entity match {
      case dte: model.DocTemplateEntity => dte.members
      case _ => Seq.empty
    }
    children
  }

  def convert(entity: model.Entity): Unit = {
    println("Processing " + entity.toString)
    
    entity match {
      case c: model.Class  => Class.createClass(c.toString, c.name, c.inTemplate.toString)
      case t: model.Trait  => {
        Class.createTrait(t.toString, t.name, t.inTemplate.toString)
        /*
        for ((p, i) <- t.typeParams.zip(0 to t.typeParams.length)) {
          val converted = convertTypeParam(p)
          newT.typeParams += converted
          converted.save
          println(converted)
        }*/
      }
      
      case x: model.RootPackage if x.isRootPackage => Class.createRootPackage(x.toString)
      case p: model.Package if p.isPackage => Class.createPackage(p.toString, p.name, p.inTemplate, p.members) //maybe members should filter out class members?
      case o: model.Object => 
        Class.createObject(
          o.toString, 
          o.name, 
          o.inTemplate.
          toString, 
          o.members, 
          o.subClasses, 
          o.inheritedFrom
        )
        //Class.create.name(o.name).typ(TypeEnum.Object).save
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

