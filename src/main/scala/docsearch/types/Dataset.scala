package docsearch.types.nondb

import collection.mutable
import scala.xml._

object Dataset {
  //String name is path#type ie. scala.mutable.map#trait
  val containers: mutable.Map[String, Container] = mutable.Map.empty
  val members: mutable.Map[String, Member] = mutable.Map.empty

  def +=(c: Container): Unit = containers += c.id + "#" + c.generalType.toString -> c

  def +=(m: Member): Unit = members += m.id -> m

  def toXML: NodeSeq =
    <dataset>
      <containers>{for ((id, c) <- containers) yield c.toXML}</containers>
      <members>{for ((id, m) <- members) yield m}</members>
    </dataset>

  def loadXML(xml: NodeSeq): Unit = {
    xml match {
      case <dataset><containers>{containers}</containers><members>{members}</members></dataset> =>
        loadContainers(containers); loadMembers(members)
      case _ => error("Invalid format of XML")
    }
  }

  private def loadContainers(xml: NodeSeq): Unit = ()

  private def loadMembers(xml: NodeSeq): Unit = ()
   // for (memberXML <- xml; member <- Member.fromXML(memberXML))
}

object loadXML extends Application {
  XML.loadFile("data.xml")
}
