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
  
  val seenHash = mutable.HashSet.empty[model.MemberEntity]
  //maybe i should use set in case there are duplicates?
  def generate(docModel: Universe) = {
    val dte = traverse(docModel.rootPackage) //linearize everything
    println("Number of stuff: " + dte.count(_=>true))
    val packages = dte.filter(x => x.isInstanceOf[model.Package])
    val classes = dte.filter(x => (x.isInstanceOf[model.Class] || x.isInstanceOf[model.Object] || x.isInstanceOf[model.Trait])).filterNot(x => x.isInstanceOf[model.Package])
    val members = dte.filter(x => x.isDef || x.isVar || x.isVal || x.isLazyVal)
    println("Number of packages: " + packages.count(_=>true))
    println("Number of classes: " + classes.count(_=>true))
    println("Number of members: " + members.count(_=>true))
    println("Intersect of packages and classes: " +  packages.intersect(classes))
    println("Intersect of packages and members: " +  packages.intersect(members))
    println("Intersect of classes and members: " +  members.intersect(classes))
    println("Number of Vals: " + dte.filter(x => x.isVal).count(_=>true))
    packages.find(x => x.toString == "_root_").map(x => Class.createRootPackage(x.toString))
    packages.sortWith((x,y)=> x.toString.length < y.toString.length) map createPersistantModel
    classes.sortWith((x,y)=> x.toString.length < y.toString.length) map createPersistantModel
    classes.map(x => Class.createRelationships(x.toString, x.asInstanceOf[model.DocTemplateEntity].parentTemplates)) //should probably cast this somewhere else
    members map Member.createMember
  }
  
  def traverse(obj: model.Entity): List[model.MemberEntity] = {
    obj match {
      case dte: model.DocTemplateEntity => dte :: dte.members.flatMap(m => seenHelper(m))
      case mem: model.MemberEntity => List(mem)
      case _ => List()
    }
  }
  
  def seenHelper(obj:model.MemberEntity): List[model.MemberEntity] = {
    if (!(seenHash contains obj)) {seenHash += obj; traverse(obj)}
    else List()
  }
/*
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
  */

  def createPersistantModel(entity: model.Entity): Unit = {
    //println("Processing " + entity.toString)
    
    entity match {
      case c: model.Class  => 
        Class.createClass(
        c.toString, 
        c.name, 
        c.inTemplate.toString)
      case t: model.Trait  => {
        Class.createTrait(t.toString, t.name, t.inTemplate.toString)
      }
      
      case x: model.RootPackage if x.isRootPackage => Class.createRootPackage(x.toString)
      case p: model.Package if p.isPackage => Class.createPackage(p.toString, p.name, p.inTemplate) //maybe members should filter out class members?
      case o: model.Object => 
        Class.createObject(
          o.toString, 
          o.name, 
          o.inTemplate.toString
        )
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

