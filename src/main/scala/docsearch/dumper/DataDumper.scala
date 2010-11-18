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
    //FIXME maybe i should use set in case there are duplicates?
    val dte = traverse(docModel.rootPackage) //linearize everything
    //FIXME Is this the best way to filter and cast? 
    val packages = dte.filter(x => x.isInstanceOf[model.Package]).map(x=>x.asInstanceOf[model.Package])
    //FIXME Maybe I can use groupBy? then i can do this just once
    val classes = dte.filter(
                            x => (x.isInstanceOf[model.Class] || 
                            x.isInstanceOf[model.Object] || 
                            x.isInstanceOf[model.Trait])).
                      filterNot(x => x.isInstanceOf[model.Package]).
                      map(x => x.asInstanceOf[model.DocTemplateEntity])
    val members = dte.filter(x => x.isDef || x.isVar || x.isVal || x.isLazyVal)
    println("now writing to db")
    
    packages.find(x => x.toString == "_root_").map(x => Class.createRootPackage(x.toString))  //create root package
    packages.sortWith((x,y)=> x.toString.length < y.toString.length) map Class.createPackage  
    classes.sortWith((x,y)=> x.toString.length < y.toString.length) map Class.createClass     
    classes.map(x => Class.createRelationships(x.toString, x.parentTemplates))
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
    if (!(seen contains obj)) {seen += obj; traverse(obj)}
    else List()
  }  

  def convertTypeParam(t: model.TypeParam): TypeParam = {
    val parser = new TPParser
    parser.parseParam(t.name) match {
      case parser.Success(param: TypeParam, _) => param
      case _ => error("could not parse type parameter '" + t.name + "'")
    }
  }
}

