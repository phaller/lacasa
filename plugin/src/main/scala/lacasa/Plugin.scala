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

    def isSetterOfObject(method: Symbol): Boolean =
      method.owner.isModuleClass && method.isSetter

    class StackLocalTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
      var insecureMethods: Set[Symbol] = Set()
      var currentMethods: List[Symbol] = List()

      override def transform(tree: Tree): Tree = tree match {
        case PackageDef(_, _) =>
          atOwner(currentOwner) { super.transform(tree) }

        case obj @ ModuleDef(mods, name, impl) =>
          println(s"checking object $name")
          println(s"sym.isModule: ${obj.symbol.isModule}")
          atOwner(currentOwner) { super.transform(impl) }

        case cls @ ClassDef(mods, name, tparams, impl) =>
          println(s"checking class $name")
          println(s"sym.isClass: ${cls.symbol.isClass}")
          println(s"sym.isModuleClass: ${cls.symbol.isModuleClass}")
          atOwner(currentOwner) { super.transform(impl) }

        case methodDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          println(s"checking method definition ${methodDef.symbol.name}")
          println(s"raw:\n${showRaw(methodDef)}")

          currentMethods = methodDef.symbol :: currentMethods
          val res = atOwner(currentOwner) { transform(rhs) }
          currentMethods = currentMethods.tail
          res

        case app @ Apply(fun, args) =>
          // step 1: fun is setter of an object -> problem
          println(s"checking apply of ${fun.symbol.name}")
          println(s"setter of object: ${isSetterOfObject(fun.symbol)}")

          if (isSetterOfObject(fun.symbol))
            insecureMethods = insecureMethods + currentMethods.head

          super.transform(tree)

        case unhandled =>
          println(s"unhandled tree $tree")
          println(s"raw:\n${showRaw(tree)}")
          super.transform(tree)
      }
    }

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        println("applying LaCasa phase...")
        val sl = new StackLocalTransformer(unit)
        sl.transform(unit.body)
        println("===================")
        println("summary of results:")
        println("insecure methods:")
        sl.insecureMethods.foreach { method => println(method) }
      }
    }
  }

}
