package tinyspores
package test

import scala.reflect._
import scala.tools.reflect.{ToolBox, ToolBoxError}


object util {

  def intercept[T <: Throwable : ClassTag](body: => Any): T = {
    try {
      body
      throw new Exception(s"Exception of type ${classTag[T]} was not thrown")
    } catch {
      case t: Throwable =>
        if (classTag[T].runtimeClass != t.getClass) throw t
        else t.asInstanceOf[T]
    }
  }

  def eval(code: String, compileOptions: String = ""): Any = {
    val tb = mkToolbox(compileOptions)
    tb.eval(tb.parse(code))
  }

  def mkToolbox(compileOptions: String = ""): ToolBox[_ <: api.Universe] = {
    val m = runtime.currentMirror
    m.mkToolBox(options = compileOptions)
  }

  def scalaBinaryVersion: String = {
    val Pattern = """(\d+\.\d+)\..*""".r
    scala.util.Properties.versionNumberString match {
      case Pattern(v) => v
      case _          => ""
    }
  }

  def toolboxClasspath = {
    val f = new java.io.File(s"tinyspores/target/scala-${scalaBinaryVersion}/classes")
    if (!f.exists) sys.error(s"output directory ${f.getAbsolutePath} does not exist.")
    f.getAbsolutePath
  }

  def expectError(errorSnippet: String, compileOptions: String = "",
                  baseCompileOptions: String = s"-cp ${toolboxClasspath}")(code: String): Unit = {
    try {
      eval(code, compileOptions + " " + baseCompileOptions)
      assert(false)
    } catch {
      case tbe: ToolBoxError =>
        tbe.getCause() match {
          case fe: scala.reflect.internal.FatalError => assert(true)
          case _ => assert(tbe.getMessage.contains(errorSnippet))
        }
    }
  }

}
