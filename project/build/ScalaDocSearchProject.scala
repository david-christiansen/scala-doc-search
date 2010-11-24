import sbt._

class ScalaDocSearchProject(info: ProjectInfo) extends DefaultWebProject(info) {
  val liftVersion = "2.1"

  val scalatoolsSnapshot =
    "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"

  // If you're using JRebel for Lift development, uncomment
  // this line
  // override def scanDirectories = Nil

  override def compileOptions = super.compileOptions ++ Seq(Unchecked)

  override def runClasspath = super.runClasspath +++ compileClasspath +++ ("lib_managed"/ "scala_2.8.0" / "lib" / "scala-compiler.jar")

  /* BEGIN COMPUTING CLASSPATH AND ARGS FOR DUMPER */
  val dumperClassPath = runClasspath.get.map(_.absolutePath).mkString(":") + ":" +
    buildScalaInstance.libraryJar.getAbsolutePath + ":" + buildScalaInstance.compilerJar.getAbsolutePath
  
  val scalaSourceDir = new java.io.File("scala-src")

  def tree(f: java.io.File): Stream[java.io.File] = {
    lazy val rest = if (f.isDirectory) f.listFiles.toStream.flatMap(tree) else Stream.empty
    Stream.cons(f, rest)
  }

  val sources = tree(scalaSourceDir).map(_.getAbsolutePath).filter(_.endsWith(".scala"))

  lazy val dump = runTask(
    Some("scala.tools.nsc.ScalaDocDumper"), compileClasspath, 
    Seq("-bootclasspath", dumperClassPath, 
        "-classpath", dumperClassPath) ++ sources
  ).dependsOn(compile) describedAs "Runs the data dumper."
  /* END DEFINING DUMPER */

override def testListeners. () 

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-mapper" % liftVersion % "compile->default",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default",
    "junit" % "junit" % "4.5" % "test->default",
    "org.scala-tools.testing" %% "specs" % "1.6.5" % "test->default",
    "com.h2database" % "h2" % "1.2.138",
    "org.scala-tools.testing" % "scalacheck_2.8.0" % "1.7",
    "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5",
    "org.scalatest" % "scalatest" % "1.2"
  ) ++ super.libraryDependencies

  val lift_postgresql = "postgresql" % "postgresql" % "8.4-701.jdbc4"
  
}
