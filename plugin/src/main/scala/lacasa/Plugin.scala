package lacasa

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.transform._
import scala.collection.{mutable, immutable}

class Plugin(val global: Global) extends NscPlugin {
  import global._

  val name = "lacasa"
  val description = "LaCasa plugin"
  val components = List[NscPluginComponent](PluginComponent, ReporterComponent)

  object PluginComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global._
    import definitions._

    override val runsAfter = List("refchecks")
    val phaseName = "lacasa"

    var insecureClasses: Set[Symbol] = Set()
    var secureClasses: Set[Symbol] = Set()

    def isKnown(cls: Symbol): Boolean =
      insecureClasses.contains(cls) || secureClasses.contains(cls)

    def isSetterOfObject(method: Symbol): Boolean =
      method.owner.isModuleClass && method.isSetter

    class ObjectCapabilitySecureTraverser(phase: ObjectCapabilitySecurePhase, unit: CompilationUnit) extends Traverser {
      var insecureMethods: Set[Symbol] = Set()
      var currentMethods: List[Symbol] = List()

      override def traverse(tree: Tree): Unit = tree match {
        case PackageDef(_, _) =>
          atOwner(currentOwner) { super.traverse(tree) }

        case obj @ ModuleDef(mods, name, impl) =>
          println(s"checking object $name")
          println(s"sym.isModule: ${obj.symbol.isModule}")
          traverse(impl)

        case cls @ ClassDef(mods, name, tparams, impl) =>
          println(s"checking class $name")
          println(s"sym.isClass: ${cls.symbol.isClass}")
          println(s"sym.isModuleClass: ${cls.symbol.isModuleClass}")
          traverse(impl)

        case templ @ Template(parents, self, body) =>
          if (parents.exists(parent => isKnown(parent.symbol) && insecureClasses.contains(parent.symbol))) {
            phase.addInsecureClass(templ.symbol.owner)
          } else {
            atOwner(currentOwner) { super.traverse(tree) }
          }

        case methodDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          println(s"checking method definition ${methodDef.symbol.name}")
          println(s"raw:\n${showRaw(methodDef)}")

          currentMethods = methodDef.symbol :: currentMethods
          traverse(rhs)
          currentMethods = currentMethods.tail

        case app @ Apply(fun, args) =>
          // step 1: fun is setter of an object -> problem
          println(s"checking apply of ${fun.symbol.name}")
          println(s"setter of object: ${isSetterOfObject(fun.symbol)}")

          if (isSetterOfObject(fun.symbol)) {
            insecureMethods = insecureMethods + currentMethods.head
            phase.addInsecureClass(currentMethods.head.owner)
          }

          super.traverse(tree)

        case unhandled =>
          println(s"unhandled tree $tree")
          println(s"raw:\n${showRaw(tree)}")
          super.traverse(tree)
      }
    }

    class ObjectCapabilitySecurePhase(prev: Phase) extends StdPhase(prev) {
      def addInsecureClass(cls: Symbol): Unit =
        insecureClasses = insecureClasses + cls

      override def apply(unit: CompilationUnit): Unit = {
        println("applying LaCasa ObjectCapabilitySecurePhase...")
        val ocst = new ObjectCapabilitySecureTraverser(this, unit)
        ocst.traverse(unit.body)
      }
    }

    override def newPhase(prev: Phase): StdPhase =
      new ObjectCapabilitySecurePhase(prev)
  }

  object ReporterComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global._
    import definitions._

    override val runsAfter = List("lacasa")
    val phaseName = "lacasareporter"

    var hasRun = false

    override def newPhase(prev: Phase): StdPhase =
      new StdPhase(prev) {
        override def apply(unit: CompilationUnit): Unit =
          if (!hasRun) {
            println("summary of results")
            println("==================")
            println("insecure classes:")
            PluginComponent.insecureClasses.foreach { cls => println(cls) }
            hasRun = true
          }
      }
  }
}
