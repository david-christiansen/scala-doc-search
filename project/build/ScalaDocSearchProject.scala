import sbt._

class ScalaDocSearchProject(info: ProjectInfo) extends DefaultWebProject(info) {
  val snapshots = ScalaToolsSnapshots
  val lift = "net.liftweb" %% "lift-mapper" % "2.1-SNAPSHOT" % "compile"
  val jetty6 = "org.mortbay.jetty" % "jetty" % "6.1.14" % "test"
  val h2 = "com.h2database" % "h2" % "1.2.121"
  val servlet = "javax.servlet" % "servlet-api" % "2.5" % "provided"
  val derby = "org.apache.derby" % "derby" % "10.2.2.0" % "runtime"
  val junit = "junit" % "junit" % "3.8.1" % "test"


}
