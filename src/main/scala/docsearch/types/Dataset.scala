package docsearch.types.nondb

import collection.mutable
import scala.xml._

object Dataset {
  val containers: mutable.Map[String, Set[Container]] = mutable.Map.empty
  val members: mutable.Map[String, Set[Member]] = mutable.Map.empty

  def +=(c: Container): Unit = {
    val current = containers.getOrElse(c.id, Set.empty)
    containers += c.id -> (current + c)
  }

  def +=(m: Member): Unit = {
    val current = members.getOrElse(m.id, Set.empty)
    members += m.id -> (current + m)
  }

  def toXML: NodeSeq =
    <dataset>
      <containers>{for ((id, cs) <- containers; c <- cs) yield c.toXML}</containers>
      <members>{for ((id, ms) <- members; m <- ms) yield m}</members>
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
}
