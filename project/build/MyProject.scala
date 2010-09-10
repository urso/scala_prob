
import sbt._

class MyProject(info:ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {

    val scalazCore = "com.googlecode.scalaz" %% "scalaz-core" % "5.0"
    val mprob          = "None" %% "scala_mprob" % "0.0.1"
    val embeddedmonads = "None" %% "embeddedmonads" % "0.0.1"

    val cont = compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.8.0")
    override def compileOptions = super.compileOptions ++ compileOptions("-P:continuations:enable")
}

