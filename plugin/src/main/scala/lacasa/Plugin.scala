package lacasa

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.transform._
import scala.collection.{mutable, immutable}

class Plugin(val global: Global) extends NscPlugin {
  import global.{log => _, _}

  val name = "lacasa"
  val description = "LaCasa plugin"
  val components = List[NscPluginComponent](PluginComponent, ReporterComponent)

  val logEnabled = System.getProperty("lacasa.plugin.logging", "false") == "true"
  def log(msg: => String): Unit = {
    if (logEnabled)
      println(msg)
  }

  object PluginComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global.{log => _, _}
    import definitions._
    import reflect.internal.Flags._

    override val runsAfter = List("refchecks")
    val phaseName = "lacasa"

    var analyzedClasses: Set[Symbol] = Set()
    var analyzedTraits: Set[Symbol] = Set()
    var analyzedObjects: Set[Symbol] = Set()
    var analyzedTempls: Set[Symbol] = Set()

    def addClass(sym: Symbol): Unit =
      analyzedClasses += sym

    def addTrait(sym: Symbol): Unit =
      analyzedTraits += sym

    def addObject(sym: Symbol): Unit =
      analyzedObjects += sym

    def addTempl(sym: Symbol): Unit =
      analyzedTempls += sym

    var mutableObjects: Set[Symbol] = Set()
    def addMutableObject(sym: Symbol): Unit =
      mutableObjects += sym

    var patterns: Map[Int, Int] = Map()
    def addPattern(num: Int): Unit = {
      val current = patterns.getOrElse(num, 0)
      patterns += (num -> (current + 1))
    }

    var patternsChecked: Map[Int, Int] = Map()
    def checkPattern(num: Int): Unit = {
      val current = patternsChecked.getOrElse(num, 0)
      patternsChecked += (num -> (current + 1))
    }

    var insecureClasses: Set[Symbol] = Set()
    // inheriting from `scala.AnyRef' is secure
    var secureClasses: Set[Symbol] = Set(AnyRefClass, ObjectClass)
    // invariant: \forall s \in deps(c). !isKnown(s)
    var deps: Map[Symbol, List[Symbol]] = Map()

    def isKnown(cls: Symbol): Boolean =
      insecureClasses.contains(cls) || secureClasses.contains(cls)

    def addInsecureClass(cls: Symbol): Unit =
      insecureClasses = insecureClasses + cls

    def mkInsecure(cls: Symbol): Unit = {
      if (deps.isDefinedAt(cls))
        deps = deps - cls
      addInsecureClass(cls)
      // go through classes with unknown status (= keys in deps)
      // if `cls' is one of the values: make insecure
      val newInsecure =
        for ((cls1, itsDeps) <- deps; if itsDeps.contains(cls)) yield cls1
      for (cls1 <- newInsecure) mkInsecure(cls1)
    }

    def isSetterOfObject(method: Symbol): Boolean =
      method.owner.isModuleClass && method.isSetter

    def isGetterOfObjectWithSetter(method: Symbol): Boolean = {
      method.owner.isModuleClass && method.isGetter &&
      method.setterIn(method.owner) != NoSymbol
    }

    class ObjectCapabilitySecureTraverser(unit: CompilationUnit) extends Traverser {
      var insecureMethods: Set[Symbol] = Set()
      var currentMethods: List[Symbol] = List()

      override def traverse(tree: Tree): Unit = tree match {

        case cls @ ClassDef(mods, name, tparams, impl) =>
          log(s"checking class $name")
          log(s"sym.isClass: ${cls.symbol.isClass}")
          log(s"sym.isModuleClass: ${cls.symbol.isModuleClass}")
          if (cls.symbol.isModuleClass) addObject(cls.symbol) else {
            if (mods hasFlag TRAIT) addTrait(cls.symbol) else addClass(cls.symbol)
          }
          traverse(impl)

        case templ @ Template(parents, self, body) =>
          val cls = templ.symbol.owner
          addTempl(cls)
          if (parents.exists(parent => isKnown(parent.symbol) && insecureClasses.contains(parent.symbol))) {
            mkInsecure(cls)
          } else {
            if (parents.exists(parent => !isKnown(parent.symbol))) {
              // for each unknown parent: create dependency
              parents.foreach { parent =>
                val parentSym = parent.symbol
                if (!isKnown(parentSym)) {
                  val currentDeps = deps.getOrElse(cls, List[Symbol]())
                  if (!currentDeps.contains(parentSym))
                    deps = deps + (cls -> (parentSym :: currentDeps))
                }
              }
            }
            body.foreach(t => traverse(t))
          }

        case vd @ ValDef(mods, name, tpt, rhs) =>
          // if vd is a var and owner is object -> keep track of object!
          if (mods.hasFlag(MUTABLE) && vd.symbol.owner.isModuleClass) {
            addMutableObject(vd.symbol.owner)
          }
          traverse(rhs)

        case methodDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          log(s"checking method definition ${methodDef.symbol.name}")
          log(s"raw:\n${showRaw(methodDef)}")

          currentMethods = methodDef.symbol :: currentMethods
          traverse(rhs)
          currentMethods = currentMethods.tail

        case app @ Apply(fun, args) =>
          // problem pattern 1: fun is setter of an object
          log(s"checking apply of ${fun.symbol.name}")
          log(s"setter of object: ${isSetterOfObject(fun.symbol)}")

          checkPattern(1)
          if (isSetterOfObject(fun.symbol) && currentMethods.nonEmpty /*TODO*/) {
            insecureMethods = insecureMethods + currentMethods.head
            mkInsecure(currentMethods.head.owner)
            addPattern(1)
          }

          traverse(fun)
          args.foreach { arg => traverse(arg) }

        case sel @ Select(obj, _) =>
          // problem pattern 2: invoke getter which also has setter
          log(s"checking select of ${sel.symbol.name}")
          log(s"getter of object: ${isGetterOfObjectWithSetter(sel.symbol)}")

          checkPattern(2)
          if (isGetterOfObjectWithSetter(sel.symbol) && currentMethods.nonEmpty /*TODO*/) {
            insecureMethods = insecureMethods + currentMethods.head
            mkInsecure(currentMethods.head.owner)
            addPattern(2)
          }

          traverse(obj)

        case Literal(any) => /* all good */

        case unhandled =>
          log(s"unhandled tree $tree")
          log(s"raw:\n${showRaw(tree)}")
          super.traverse(tree)
      }
    }

    class ObjectCapabilitySecurePhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        log("applying LaCasa ObjectCapabilitySecurePhase...")
        val ocst = new ObjectCapabilitySecureTraverser(unit)
        ocst.traverse(unit.body)
      }
    }

    override def newPhase(prev: Phase): StdPhase =
      new ObjectCapabilitySecurePhase(prev)
  }

  object ReporterComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global.{log => _, _}
    import definitions._

    override val runsAfter = List("lacasa")
    val phaseName = "lacasareporter"

    var hasRun = false

    override def newPhase(prev: Phase): StdPhase =
      new StdPhase(prev) {
        override def apply(unit: CompilationUnit): Unit =
          if (!hasRun) {
            hasRun = true
            reporter.echo("LaCasa plugin ran successfully")
            reporter.echo("#classes analyzed: " + PluginComponent.analyzedClasses.size)
            reporter.echo("#traits analyzed: " + PluginComponent.analyzedTraits.size)
            reporter.echo("#objects analyzed: " + PluginComponent.analyzedObjects.size)
            reporter.echo("#templs analyzed: " + PluginComponent.analyzedTempls.size)

            reporter.echo(s"#objects mutable: ${PluginComponent.mutableObjects.size}")
            PluginComponent.mutableObjects.foreach { sym =>
              reporter.echo(s"object ${sym.name}")
            }

            reporter.echo(s"patterns checked: ${PluginComponent.patternsChecked}")
            reporter.echo(s"patterns found: ${PluginComponent.patterns}")

            //reporter.warning(NoPosition, "reporter.warning: LaCasa plugin ran successfully")

            log("summary of results")
            log("==================")
            log("insecure classes:")
            PluginComponent.insecureClasses.foreach { cls => log(cls.toString) }

            if (PluginComponent.deps.keySet.nonEmpty) {
              log("\nunresolved dependencies:")
              log(PluginComponent.deps.toString)
            }

            if (PluginComponent.insecureClasses.nonEmpty) {
              val classNames = PluginComponent.insecureClasses.map(cls => cls.toString)
              log(s"""error: insecure classes: ${classNames.mkString(",")}""")
              error(s"""insecure classes: ${classNames.mkString(",")}""")
            }
          }
      }
  }
}
