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

  
  val seen = mutable.HashSet.empty[model.MemberEntity]
  
  def generate(docModel: Universe) = {
    val dte = flatten(docModel.rootPackage) //linearize everything
    //FIXME Is this the best way to filter and cast? 
    val packages = dte filter (_.isInstanceOf[model.Package]) map (_.asInstanceOf[model.Package])
    //FIXME Maybe I can use groupBy? then i can do this just once
    val classes = dte.filter(
                            x => (x.isInstanceOf[model.Class] || 
                            x.isInstanceOf[model.Object] || 
                            x.isInstanceOf[model.Trait])).
                      filterNot(_.isInstanceOf[model.Package]).
                      map(_.asInstanceOf[model.DocTemplateEntity])
    val members = dte.filter(x => x.isDef || x.isVar || x.isVal || x.isLazyVal)
    println("now writing to db")
    
    println("Saving root package to db")
    packages.find(x => x.toString == "_root_").map(x => Class.createRootPackage(x.toString))  //create root package
    println("Saving packages to db")
    packages.sortWith((x,y)=> x.toString.length < y.toString.length) foreach Class.createPackage  
    println("Saving classes, traits, etc to db")
    classes.sortWith((x,y)=> x.toString.length < y.toString.length) foreach Class.createClass
    println("Saving inheritance graph to db")
    classes.map(x => Class.createRelationships(x.toString, x.parentTemplates))
    
    val memCount = members.length
    println("Putting " + memCount + " members and their types in db")
    for ((mem, thisMem) <- members.zipWithIndex) { 
      if ((thisMem + 1) % 10000 == 0) 
        println(" Reached member " + (thisMem + 1) + " of " + memCount + " (" + (thisMem * 100 / memCount) + ")%")
      Member.createMember(mem)
    }
  }
  
  def flatten(obj: model.Entity): List[model.MemberEntity] = {
    obj match {
      case dte: model.DocTemplateEntity => dte :: dte.members.flatMap(m => seenHelper(m))
      case mem: model.MemberEntity => List(mem)
      case _ => List()
    }
  }
  
  def seenHelper(obj:model.MemberEntity): List[model.MemberEntity] = {
    if (!(seen contains obj)) {seen += obj; flatten(obj)}
    else List()
  }  


}

