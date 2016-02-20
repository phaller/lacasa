package lacasa

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.transform._
import scala.collection.{mutable, immutable}

class Plugin(val global: Global) extends NscPlugin {
  import global._

  val name = "lacasa"
  val description = "LaCasa plugin"
  val components = List[NscPluginComponent](PluginComponent)

  object PluginComponent extends NscPluginComponent with TypingTransformers {
    val global = Plugin.this.global
    import global._
    import definitions._

    override val runsAfter = List("refchecks")
    val phaseName = "lacasa"

    class StackLocalTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = tree match {
        case t @ Template(_, _, _) =>
          println(s"checking template ${t.symbol.name}")
          atOwner(currentOwner) { super.transform(tree) }
        case _ =>
          super.transform(tree)
      }
    }

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        println("applying LaCasa phase...")
        val sl = new StackLocalTransformer(unit)
        sl.transform(unit.body)
      }
    }
  }

}
