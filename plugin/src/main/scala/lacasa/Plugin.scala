/**
 * Copyright (C) 2015-2016 Philipp Haller
 */
package lacasa

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.transform._
import scala.collection.{mutable, immutable}

import Util._


class Plugin(val global: Global) extends NscPlugin {
  import global.{log => _, _}

  val name = "lacasa"
  val description = "LaCasa plugin"
  val components = List[NscPluginComponent](new StackConfinement(global), PluginComponent, ReporterComponent)

  val boxModule = rootMirror.getModuleByName(newTermName("lacasa.Box"))
  val boxCreationMethod = boxModule.moduleClass.tpe.member(newTermName("mkBox"))
  val boxClass = rootMirror.getClassByName(newTermName("lacasa.Box"))
  val boxOpenMethod = boxClass.tpe.member(newTermName("open"))
  val boxSwapMethod = boxClass.tpe.member(newTermName("swap"))

  object PluginComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global.{log => _, _}
    import definitions._
    import reflect.internal.Flags._

    override val runsAfter = List("lacasa-stackconfinement")
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
    def addMutableObject(sym: Symbol): Unit = {
      mutableObjects += sym

      // go through classes with unknown status (= keys in depsGlobal)
      // if `sym' is one of the values: make insecure
      val newInsecure =
        for ((cls1, itsDeps) <- depsGlobal; if itsDeps.contains(sym)) yield cls1
      for (cls1 <- newInsecure) mkInsecure(cls1)
    }

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
    var secureClasses: Set[Symbol] = Set(AnyRefClass, ObjectClass, SerializableClass, JavaSerializableClass)

    // invariant: \forall s \in deps(c). !isKnown(s)
    var deps: Map[Symbol, List[Symbol]] = Map()

    // invariant: \forall s \in depsGlobal(c). !isKnownMutable(s)
    var depsGlobal: Map[Symbol, List[Symbol]] = Map()

    var insecureMethods: Set[Symbol] = Set()

    def isKnown(cls: Symbol): Boolean =
      insecureClasses.contains(cls) || secureClasses.contains(cls) || secureStrictClassNames.contains(cls.name.toString)

    def isKnownMutable(obj: Symbol): Boolean =
      mutableObjects.contains(obj)

    def addInsecureClass(cls: Symbol): Unit =
      insecureClasses = insecureClasses + cls

    def mkInsecure(cls: Symbol): Unit = {
      if (deps.isDefinedAt(cls))
        deps = deps - cls
      if (depsGlobal.isDefinedAt(cls))
        depsGlobal = depsGlobal - cls

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
      method.setter(method.owner) != NoSymbol
    }

    def dependOn(cls: Symbol, method: Symbol, other: Symbol): Unit = {
      if (isKnown(other) && insecureClasses.contains(other)) {
        insecureMethods = insecureMethods + method
        mkInsecure(cls)
      } else if (other != cls && !isKnown(other)) {
        val currentDeps = deps.getOrElse(cls, List[Symbol]())
        if (!currentDeps.contains(other))
          deps = deps + (cls -> (other :: currentDeps))
      }
    }

    //-------------//
    // strict ocap //
    //-------------//

    var insecureStrictClasses: Set[Symbol] = Set()
    // inheriting from `scala.AnyRef' is secure
    var secureStrictClasses: Set[Symbol] = Set(AnyRefClass, ObjectClass, SerializableClass, JavaSerializableClass)
    var secureStrictClassNames: Set[String] = Set("Product")

    val cbf = rootMirror.getClassByName(newTermName("scala.collection.generic.CanBuildFrom"))

    /*val mathPackage   = rootMirror.getPackage(TermName("scala.math"))
    val numericModule = rootMirror.getModuleByName(TermName("scala.math.Numeric"))
    val staticsModule = rootMirror.getModuleByName(TermName("scala.runtime.Statics"))

    val secureScalaModules: Set[Symbol] = Set(
      rootMirror.getModuleByName(TermName("scala.Array")),
      rootMirror.getModuleByName(TermName("scala.reflect.ClassTag")),
      rootMirror.getModuleIfDefined(TermName("scala.reflect.ManifestFactory")),
      rootMirror.getModuleIfDefined(TermName("scala.reflect.runtime.package")),
      rootMirror.getModuleIfDefined(TermName("scala.math.package")),
      rootMirror.getModuleIfDefined(TermName("scala.math.Ordering"))
    )*/

    val okScalaRuntimeModules: Set[String] = Set(
      "scala",
      "scala.package",
      "scala.Int",
      "scala.Array",
      "scala.Function",
      "scala.PartialFunction",
      "scala.Symbol",
      "scala.StringContext",
      "scala.Specializable",
      "scala.Predef",
      "scala.Predef.Ensuring",
      "scala.Predef.any2stringadd",
      "scala.Predef.StringAdd",
      "scala.Predef.StringFormat",
      "scala.Predef.DummyImplicit",
      "scala.Predef.RichException",
      "scala.Predef.ArrowAssoc",
      "scala.runtime.ScalaRunTime",
      "scala.runtime.StringAdd",
      "scala.runtime.Tuple2Zipped",
      "scala.runtime.Tuple2Zipped.Ops",
      "scala.runtime.Tuple3Zipped",
      "scala.runtime.Tuple3Zipped.Ops",
      "scala.runtime.RichByte",
      "scala.runtime.RichShort",
      "scala.runtime.RichInt",
      "scala.runtime.RichLong",
      "scala.runtime.RichChar",
      "scala.runtime.RichFloat",
      "scala.runtime.RichDouble",
      "scala.runtime.StringFormat",
      "scala.runtime.Statics",
      "scala.compat.Platform",
      "scala.reflect.ClassTag",
      "scala.reflect.ManifestFactory",
      "scala.reflect.runtime.package",
      "scala.math",
      "scala.math.package",
      "scala.math.Numeric",
      "scala.math.Ordering",
      "scala.math.Equiv",
      "scala.math.BigDecimal",
      "scala.collection.generic.CanBuildFrom"
    )

    /*val secureScalaCollectionModules: Set[Symbol] = Set(
      rootMirror.getModuleIfDefined(TermName("scala.collection")),
      rootMirror.getModuleIfDefined(TermName("scala.collection.Iterator")),
      rootMirror.getModuleIfDefined(TermName("scala.collection.immutable")),
      rootMirror.getModuleIfDefined(TermName("scala.collection.immutable.List")),
      rootMirror.getModuleIfDefined(TermName("scala.collection.immutable.Map")),
      rootMirror.getModuleIfDefined(TermName("scala.collection.immutable.Stream")),
      rootMirror.getModuleIfDefined(TermName("scala.collection.immutable.Stream.cons"))
    )*/

    val okScalaCollectionModules: Set[String] = Set(
      "scala.collection",
      "scala.collection.Iterator",
      "scala.collection.immutable",
      "scala.collection.immutable.List",
      "scala.collection.immutable.Map",
      "scala.collection.immutable.Stream",
      "scala.collection.immutable.Stream.cons"
    )

    /*val secureScalaLibraryModules: Set[Symbol] = Set(
      rootMirror.getModuleIfDefined(TermName("scala.Option")),
      rootMirror.getModuleIfDefined(TermName("scala.util")),
      rootMirror.getModuleIfDefined(TermName("scala.util.Sorting")),
      rootMirror.getModuleIfDefined(TermName("scala.util.control.NonFatal")),
      rootMirror.getModuleIfDefined(TermName("scala.io")),
      rootMirror.getModuleIfDefined(TermName("scala.io.Source")),
      rootMirror.getModuleIfDefined(TermName("scala.sys.package")),
      rootMirror.getModuleIfDefined(TermName("scala.concurrent.Future")),
      rootMirror.getModuleIfDefined(TermName("scala.concurrent.Await")),
      rootMirror.getModuleIfDefined(TermName("scala.concurrent.ExecutionContext.Implicits")),
      rootMirror.getModuleIfDefined(TermName("scala.concurrent.duration.package")),
      rootMirror.getModuleIfDefined(TermName("scala.concurrent.duration.Duration"))
    )*/

    val okScalaLibraryModules: Set[String] = Set(
      "scala.Option",
      "scala.util",
      "scala.util.Sorting",
      "scala.util.control.NonFatal",
      "scala.io",
      "scala.io.Source",
      "scala.sys.package",
      "scala.concurrent.Future",
      "scala.concurrent.Await",
      "scala.concurrent.ExecutionContext.Implicits",
      "scala.concurrent.duration.package",
      "scala.concurrent.duration.Duration"
    )

    /*val secureJavaModules: Set[Symbol] = Set(
      rootMirror.getModuleByName(TermName("java.lang.Runtime")),
      rootMirror.getModuleByName(TermName("java.lang.System")),
      rootMirror.getModuleByName(TermName("java.lang.Thread")),
      rootMirror.getModuleByName(TermName("java.lang.Class")),
      rootMirror.getModuleByName(TermName("java.lang.Void"))
    )*/

    val okJavaModules: Set[String] = Set(
      "java.lang.Runtime",
      "java.lang.System",
      "java.lang.Thread",
      "java.lang.Class",
      "java.lang.Void",
      "java.lang.Byte",
      "java.lang.Short",
      "java.lang.Integer",
      "java.lang.Long",
      "java.lang.Float",
      "java.lang.Double",
      "java.lang.Boolean",
      "java.lang.Character",
      "java.lang.String",
      "scala.math.BigInt",
      "java.lang.reflect.Array",
      "java.util.Arrays",
      "java.util.regex.Pattern",
      "java.util.concurrent.atomic.AtomicReferenceFieldUpdater"
    )

    val okLaCasaModules: Set[String] = Set(
      "lacasa.Box",
      "lacasa.System",
      "lacasa.OnlyNothing",
      "lacasa.doNothing",
      "lacasa.sleep",
      "scala.spores.package",
      "scala.spores"
    )

    val okModules =
      okLaCasaModules ++
      okJavaModules ++
      okScalaRuntimeModules ++
      okScalaLibraryModules ++
      okScalaCollectionModules


    /*val secureStrictModules: Set[Symbol] = Set(ScalaRunTimeModule.moduleClass, RuntimePackage.moduleClass, ScalaPackageClass, PredefModule.moduleClass, mathPackage.moduleClass, numericModule.moduleClass, staticsModule.moduleClass) ++
      secureScalaModules.map(_.moduleClass) ++
      //secureScalaCollectionModules.map(_.moduleClass) ++
      //secureScalaLibraryModules.map(_.moduleClass) ++
      secureJavaModules.map(_.moduleClass)
     */

    // invariant: \forall s \in depsStrict(c). !isKnownStrict(s)
    var depsStrict: Map[Symbol, List[Symbol]] = Map()

    // all type arguments to mkBox[C] { ... } *must* be ocap, otherwise error!
    var requiredOcapClasses: Set[Symbol] = Set()
    def requireOcap(s: Symbol): Unit = {
      requiredOcapClasses += s
    }

    def isKnownStrict(cls: Symbol): Boolean =
      insecureStrictClasses.contains(cls) || secureStrictClasses.contains(cls) || secureStrictClassNames.contains(cls.name.toString)

    def mkInsecureStrict(cls: Symbol): Unit = {
      if (depsStrict.isDefinedAt(cls))
        depsStrict = depsStrict - cls

      insecureStrictClasses = insecureStrictClasses + cls

      // go through classes with unknown status (= keys in depsStrict)
      // if `cls' is one of the values: make insecure
      val newInsecure =
        for ((cls1, itsDeps) <- depsStrict; if itsDeps.contains(cls)) yield cls1
      for (cls1 <- newInsecure) mkInsecureStrict(cls1)
    }

    def dependStrictOn(cls: Symbol, other: Symbol): Unit = {
      if (isKnownStrict(other) && insecureStrictClasses.contains(other)) {
        mkInsecureStrict(cls)
      } else if (other != cls && !isKnownStrict(other)) {
        val currentDeps = depsStrict.getOrElse(cls, List[Symbol]())
        if (!currentDeps.contains(other))
          depsStrict = depsStrict + (cls -> (other :: currentDeps))
      }
    }

    val debugName = "AggregationOperation"

    /*abstract class TypingTraverser(unit: CompilationUnit) extends Traverser {
      var localTyper: analyzer.Typer =
        analyzer.newTyper(analyzer.rootContextPostTyper(unit, EmptyTree))
      protected var curTree: Tree = _

      override final def atOwner(owner: Symbol)(trans: => Unit): Unit = atOwner(curTree, owner)(trans)

      def atOwner(tree: Tree, owner: Symbol)(trans: => Unit): Unit = {
        val savedLocalTyper = localTyper
        localTyper = localTyper.atOwner(tree, if (owner.isModule) owner.moduleClass else owner)
        super.atOwner(owner)(trans)
        localTyper = savedLocalTyper
      }

      override def traverse(tree: Tree): Unit = {
        curTree = tree
        tree match {
          case Template(_, _, _) =>
            // enter template into context chain
            atOwner(currentOwner) { super.traverse(tree) }
          case PackageDef(_, _) =>
            atOwner(tree.symbol) { super.traverse(tree) }
          case _ =>
            super.traverse(tree)
        }
      }
    }*/

    var classesAccessingObjects: Set[Symbol] = Set()
    var accessedObjects: Set[String] = Set()

    // uses isKnownStrict, insecureStrictClasses, mkInsecureStrict, depsStrict, dependStrictOn
    class OcapStrictTraverser(unit: CompilationUnit) extends Traverser {
      var currentMethods: List[Symbol] = List()
      override def traverse(tree: Tree): Unit = tree match {

        case templ @ Template(parents, self, body) =>
          val cls = templ.symbol.owner
          if (!cls.isModuleClass) {
            log(s"STRICT: checking ${cls.fullName}")
            if (parents.exists(parent => isKnownStrict(parent.symbol) && insecureStrictClasses.contains(parent.symbol))) {
              log(s"STRICT: make insecure ${cls.fullName}")
              if (cls.name.toString.startsWith(debugName))
                println(s"LACASA: make insecure ${cls.name} because of insecure parent")
              mkInsecureStrict(cls)
            } else {
              if (parents.exists(parent => !isKnownStrict(parent.symbol))) {
                log(s"STRICT: there is unknown parent of ${cls.fullName}!")
                // for each unknown parent: create dependency
                parents.foreach { parent =>
                  val parentSym = parent.symbol
                  if (!isKnownStrict(parentSym)) {
                    if (cls.name.toString.startsWith(debugName))
                      println(s"LACASA: add dep for ${cls.name} on ${parentSym.name} because of inheritance")
                    val currentDeps = depsStrict.getOrElse(cls, List[Symbol]())
                    if (!currentDeps.contains(parentSym))
                      depsStrict = depsStrict + (cls -> (parentSym :: currentDeps))
                  }
                }
              }
            }
          }
          body.foreach(t => traverse(t))

        case methodDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          log(s"STRICT ${methodDef.symbol.name} checking method definition ${methodDef.symbol.name}")
          log(s"STRICT raw:\n${showRaw(methodDef)}")
          currentMethods = methodDef.symbol :: currentMethods
          traverse(rhs)
          currentMethods = currentMethods.tail

        case TypeApply(fun, args) =>
          // check for selection of box creation method: Box.mkBox[C] { ... }
          if (fun.symbol == boxCreationMethod)
            requireOcap(args.head.symbol)

        case Apply(fun, args) if fun.symbol == boxOpenMethod =>
          val leakChecker = new Traverser {
            override def traverse(tree: Tree): Unit = tree match {
              case sel @ Select(obj, _) =>
                val potentialMod = sel.symbol.owner
                if (potentialMod.isModuleClass && potentialMod.isSynthetic) {
                  // synthetic modules are secure, do nothing
                } else if (okModules.contains(potentialMod.fullName.toString)) {
                  log(s"STRICT SECURE MODULE SELECTED $potentialMod: $sel")
                } else if (potentialMod.isModuleClass) {
                  // if type of sel is CanBuildFrom we allow it
                  val isCBF = {
                    val cbfUseWithTypeArgs = sel.tpe.finalResultType
                    val cbfUseTypeSym = cbfUseWithTypeArgs.typeConstructor.typeSymbol
                    cbfUseTypeSym == cbf
                  }
                  if (!isCBF) {
                    log(s"STRICT INSECURE: insecure selection on object: $sel")
                    reporter.error(sel.pos, s"no access to top-level object ${potentialMod.fullName} allowed")
                  }
                } else {
                  traverse(obj)
                }

              case nt @ New(id) =>
                id match {
                  case tpt @ TypeTree() => // empty TypeTree: check original...
                    tpt.original match {
                      case AppliedTypeTree(classToCheck, tpeArgs) =>
                        val tpeArgsToCheck = tpeArgs.map {
                          case mt @ TypeTree() => mt.original
                          case nonMt => nonMt
                        }
                        requireOcap(classToCheck.symbol)
                        tpeArgsToCheck.foreach { tparg =>
                          if (tparg != null && tparg.symbol != null && !tparg.symbol.isAbstractType)
                            requireOcap(tparg.symbol)
                        }
                      case _ => /* do nothing */
                    }

                  case other => // non-empty TypeTree: check its symbol directly
                    requireOcap(other.symbol)
                }

              case other => super.traverse(other)
            }
          }
          args.foreach {
            case block @ Block(_, _) => leakChecker.traverse(block)
            case _ => // do not traverse
          }
          traverse(fun)

        // STRICT: selecting member of object is insecure
        case sel @ Select(obj, mem) =>
          if (currentMethods.nonEmpty && !currentMethods.head.owner.isModuleClass) {
            val potentialMod = sel.symbol.owner
            if (potentialMod.isModuleClass && potentialMod.isSynthetic) {
              // synthetic modules are secure, do nothing
            } else if (/*secureStrictModules.contains(sel.symbol.owner)*/
                okModules.contains(potentialMod.fullName.toString)) {
              log(s"STRICT SECURE MODULE SELECTED ${obj.symbol}: $sel")
            } else if (potentialMod.isModuleClass) {
              val cls = currentMethods.head.owner

              // if type of sel is CanBuildFrom we allow it
              val isCBF = {
                val cbfUseWithTypeArgs = sel.tpe.finalResultType
                val cbfUseTypeSym = cbfUseWithTypeArgs.typeConstructor.typeSymbol
                cbfUseTypeSym == cbf
              }

              if (!isCBF) {
                log(s"STRICT INSECURE ${cls.name}: insecure selection on object: $sel")
                if (cls.name.toString.startsWith(debugName)) {
                  println(s"LACASA: marked ${cls.name} insecure because of selection $sel")
                  println(s"""LACASA: obj.symbol = ${obj.symbol}, okModules = ${okModules.mkString(",")}""")
                  println(s"LACASA: sel.symbol.owner = ${sel.symbol.owner.fullName}")
                }
                mkInsecureStrict(cls)
                classesAccessingObjects += cls
                accessedObjects += potentialMod.fullName.toString
              }
            } else {
              traverse(obj)
            }
          }

        // STRICT: instantiating insecure class is insecure
        case nt @ New(id) =>
          if (currentMethods.nonEmpty && !currentMethods.head.owner.isModuleClass) {
            val cls = currentMethods.head.owner
            id match {
              case tpt @ TypeTree() => // empty TypeTree: check original...
                val orig = tpt.original
                orig match {
                  case AppliedTypeTree(classToCheck, tpeArgs) =>
                    val tpeArgsToCheck = tpeArgs.map {
                      case mt @ TypeTree() => mt.original
                      case nonMt => nonMt
                    }

                    if (classToCheck.symbol != cls) {
                      if (cls.name.toString.startsWith(debugName))
                        println(s"LACASA: add dep new-1 for ${cls.name} on ${classToCheck.symbol.name} because of $nt")
                      dependStrictOn(cls, classToCheck.symbol)
                    }

                    tpeArgsToCheck.foreach { tparg =>
                      if (tparg != null && tparg.symbol != null && !tparg.symbol.isAbstractType) {
                        if (cls.name.toString.startsWith(debugName))
                          println(s"LACASA: add dep new-2 for ${cls.name} on ${tparg.symbol.name} because of $nt")
                        dependStrictOn(cls, tparg.symbol)
                      }
                    }
                  case _ => /* do nothing */
                }

              case other => // non-empty TypeTree: check its symbol directly
                if (other.symbol != cls) {
                  if (cls.name.toString.startsWith(debugName))
                    println(s"LACASA: add dep new-3 for ${cls.name} on ${other.symbol.name} because of $nt")
                  dependStrictOn(cls, other.symbol)
                }
            }
          }

        case _ =>
          super.traverse(tree)
      }
    }

    class OcapTraverser(unit: CompilationUnit) extends Traverser {
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
          if (isSetterOfObject(fun.symbol) && currentMethods.nonEmpty /*TODO*/ &&
              !currentMethods.head.owner.isModuleClass) {
            insecureMethods = insecureMethods + currentMethods.head
            mkInsecure(currentMethods.head.owner)
            addPattern(1)
          }

          def ownerObject(method: Symbol): Option[Symbol] =
            if (method.owner.isModuleClass) Some(method.owner) else None

          // problem pattern 3: fun is method of mutable object
          checkPattern(3)
          val ownerOpt = ownerObject(fun.symbol)
          log(s"ownerIsObject: $ownerOpt")
          if (ownerOpt.nonEmpty && currentMethods.nonEmpty /*TODO*/ &&
              !currentMethods.head.owner.isModuleClass) {
            val owner = ownerOpt.get
            if (/*secureStrictModules.contains(owner)*/
                okModules.contains(owner.fullName.toString)) {
              log(s"STRICT SECURE MODULE SELECTED $owner: $fun")
            } else {
              val cls = currentMethods.head.owner
              log(s"owner isKnownMutable: ${isKnownMutable(owner)}")
              if (!isKnownMutable(owner)) {
                val currentDeps = depsGlobal.getOrElse(cls, List[Symbol]())
                if (!currentDeps.contains(owner))
                  depsGlobal = depsGlobal + (cls -> (owner :: currentDeps))
                log(s"added global dep on ${owner.name}")
              } else {
                insecureMethods = insecureMethods + currentMethods.head
                mkInsecure(cls)
                addPattern(3)
              }
            }
          }

          traverse(fun)
          args.foreach { arg => traverse(arg) }

        case sel @ Select(obj, _) =>
          // problem pattern 2: invoke getter which also has setter
          log(s"checking select of ${sel.symbol.name}")
          log(s"getter of object: ${isGetterOfObjectWithSetter(sel.symbol)}")

          checkPattern(2)
          if (isGetterOfObjectWithSetter(sel.symbol) && currentMethods.nonEmpty /*TODO*/ &&
              !currentMethods.head.owner.isModuleClass) {
            insecureMethods = insecureMethods + currentMethods.head
            mkInsecure(currentMethods.head.owner)
            addPattern(2)
          }

          traverse(obj)

        case New(id) =>
          // problem pattern 4: create instance of insecure class
          log(s"checking constructor invocation $id")

          if (currentMethods.nonEmpty) {
            val cls = currentMethods.head.owner
            val other = id.symbol
            // check if instantiated class (other) is insecure
            if (insecureClasses.contains(other)) {
              insecureMethods = insecureMethods + currentMethods.head
              mkInsecure(cls)
            } else if (!isKnown(other)) {
              val currentDeps = deps.getOrElse(cls, List[Symbol]())
              if (!currentDeps.contains(other)) {
                deps = deps + (cls -> (other :: currentDeps))
              }
            }
          }

          checkPattern(4)
          if (currentMethods.nonEmpty /*TODO*/ && !currentMethods.head.owner.isModuleClass) {
            val cls = currentMethods.head.owner
            id match {
              case tpt @ TypeTree() =>
                // empty TypeTree: check original...
                val orig = tpt.original
                orig match {
                  case AppliedTypeTree(classToCheck, tpeArgs) =>
                    val tpeArgsToCheck = tpeArgs.map {
                      case mt @ TypeTree() => mt.original
                      case nonMt => nonMt
                    }
                    dependOn(cls, currentMethods.head, classToCheck.symbol)
                    tpeArgsToCheck.foreach { tparg =>
                      if (tparg != null && tparg.symbol != null && !tparg.symbol.isAbstractType)
                        dependOn(cls, currentMethods.head, tparg.symbol)
                    }

                  case _ => /* do nothing */
                }

              case other =>
                // non-empty TypeTree: check its symbol directly
                dependOn(cls, currentMethods.head, other.symbol)
            }
          }

        case Literal(any) => /* all good */

        case unhandled =>
          log(s"unhandled tree $tree")
          log(s"raw:\n${showRaw(tree)}")
          super.traverse(tree)
      }
    }

    class OcapPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        log("applying LaCasa OcapPhase...")
        val oct = new OcapTraverser(unit)
        oct.traverse(unit.body)

        val ocst = new OcapStrictTraverser(unit)
        ocst.traverse(unit.body)
      }
    }

    override def newPhase(prev: Phase): StdPhase =
      new OcapPhase(prev)
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
        import PluginComponent._

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

            reporter.echo(s"#insecure classes: ${PluginComponent.insecureClasses.size}")
            reporter.echo("insecure classes:")
            PluginComponent.insecureClasses.foreach { cls => reporter.echo(cls.fullName) }

            reporter.echo(s"#strict insecure classes: ${PluginComponent.insecureStrictClasses.size}")
            reporter.echo(s"#classes insecure due to object accesses: ${classesAccessingObjects.size}")
            reporter.echo(s"#different accessed objects: ${accessedObjects.size}")
            reporter.echo(s"accessed objects:")
            accessedObjects.foreach { obj => reporter.echo(obj) }

            reporter.echo("################################")
            reporter.echo("################################")
            reporter.echo("################################")
            reporter.echo("strict insecure classes:")
            PluginComponent.insecureStrictClasses.foreach { cls => reporter.echo(cls.fullName) }

            requiredOcapClasses.foreach { cls =>
              if (insecureClasses.contains(cls))
                error(s"$cls not ocap!")
            }

            //reporter.warning(NoPosition, "reporter.warning: LaCasa plugin ran successfully")

            log("summary of results")
            log("==================")
            log("insecure classes:")
            PluginComponent.insecureClasses.foreach { cls => log(cls.toString) }

            if (PluginComponent.deps.keySet.nonEmpty) {
              log("\nunresolved dependencies:")
              log(PluginComponent.deps.toString)
            }

            if (PluginComponent.depsGlobal.keySet.nonEmpty) {
              log("\nunresolved global dependencies:")
              log(PluginComponent.depsGlobal.toString)
            }

            if (PluginComponent.insecureClasses.nonEmpty) {
              val classNames = PluginComponent.insecureClasses.map(cls => cls.toString)
              log(s"""error: insecure classes: ${classNames.mkString(",")}""")
            }
          }
      }
  }
}
