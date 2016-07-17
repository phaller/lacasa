package lacasa

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.transform._
import scala.collection.{mutable, immutable}

import Util._


/** Stack confinement checker
  */
class StackConfinement(val global: Global) extends NscPluginComponent {
  import global.{log => _, _}
  import definitions._
  import reflect.internal.Flags._

  override val runsAfter = List("refchecks")
  val phaseName = "lacasa-stackconfinement"

  val boxModule = rootMirror.getModuleByName(newTermName("lacasa.Box"))
  val boxCreationMethod = boxModule.moduleClass.tpe.member(newTermName("mkBox"))
  val boxClass = rootMirror.getClassByName(newTermName("lacasa.Box"))
  val boxOpenMethod = boxClass.tpe.member(newTermName("open"))
  val boxSwapMethod = boxClass.tpe.member(newTermName("swap"))

  class SCTraverser(unit: CompilationUnit) extends Traverser {
    var currentMethods: List[Symbol] = List()
    val stackConfined: List[Type] = List(typeOf[lacasa.Box[Any]], typeOf[lacasa.CanAccess])
    private val ctrlThrowableClass = rootMirror.getClassByName(newTermName("scala.util.control.ControlThrowable"))
    private val ctrlThrowableTpe = typeOf[scala.util.control.ControlThrowable]
    private val uncheckedCatchMethod = boxModule.moduleClass.tpe.member(newTermName("uncheckedCatchControl"))

    /* This method checks exception handlers which catch an exception of type
     * `ControlThrowable` or a subtype thereof.
     *
     * For an exception handler to be accepted by the checker its first statement
     * must satisfy one of the following rules:
     * - it throws an exception of a subtype of the caught exception
     * - escape hatch: the first statement of the exception handler is
     *   `uncheckedCatchControl`
     *
     * @param casedef    is only used for providing a position when reporting errors
     * @param thenbody   body of exception handler
     * @param caughtTpe  type of the exception that has been caught (subtype of
     *                   `ControlThrowable`)
     */
    def checkPropagation(casedef: CaseDef, thenbody: Tree, caughtTpe: Type): Unit = thenbody match {
      case Block((sel @ Select(_, _)) :: moreStats, theExpr) =>
        log(s"sel.symbol: ${sel.symbol}")
        log(s"symbol of uncheckedCatch: $uncheckedCatchMethod")
        if (sel.symbol == uncheckedCatchMethod) {
          // OK: escape hatch used
        } else {
          reporter.error(casedef.pos, s"caught ControlThrowable of type $caughtTpe not propagated")
        }

      case Block(Throw(theExc) :: moreStats, theExpr) =>
        log(s"type of thrown exception: ${theExc.tpe}")
        if (theExc.tpe <:< caughtTpe) {
          // OK if type of thrown exception subtype of caught exception
          log(s"caught tpe: $caughtTpe")
          log(s"theExc.tpe: ${theExc.tpe}")
        } else {
          reporter.error(casedef.pos, s"caught ControlThrowable of type $caughtTpe not propagated")
        }

      case sel @ Select(_, _) =>
        if (sel.symbol == uncheckedCatchMethod) {
          // OK: escape hatch used
        } else {
          reporter.error(casedef.pos, s"caught ControlThrowable of type $caughtTpe not propagated")
        }

      case other =>
        reporter.error(casedef.pos, s"caught ControlThrowable of type $caughtTpe not propagated")
    }

    override def traverse(tree: Tree): Unit = tree match {
      case ClassDef(mods, name, tparams, impl) =>
        log(s"checking class $name")
        traverse(impl)

      case Template(parents, self, body) =>
        body.foreach(traverse)

      case ValDef(mods, name, tpt, rhs) =>
        traverse(rhs)

      case methodDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        log(s"checking method definition ${methodDef.symbol.name}")
        // find out if this is a ctor of a spore or closure
        // TODO: is there a better way than string comparison with "anon"?
        if (methodDef.symbol.owner.fullName.toString.contains("anon") &&
          methodDef.symbol.name == termNames.CONSTRUCTOR) {
          // ctor of spore or closure: skip
        } else {
          currentMethods = methodDef.symbol :: currentMethods
          traverse(rhs)
          currentMethods = currentMethods.tail
        }

      case Apply(Select(New(id), _), args) =>
        log(s"checking constructor invocation $id")
        args.foreach { arg =>
          if (arg.symbol != null && stackConfined.exists(t => arg.symbol.tpe.finalResultType <:< t)) {
            reporter.error(arg.pos, s"$arg passed as ctor argument, but ${arg.symbol.tpe.finalResultType} confined to stack")
          }
        }

      case Apply(nested @ Apply(Apply(fun, args1), args2), List(b @ Block(stats, expr))) if fun.symbol == boxSwapMethod =>
        log(s"checking apply of ${fun.symbol.name}")
        // traverse only continuation closure
        // constraints for args1 and args2 checked elsewhere
        traverse(b)

      // apply may be a setter
      case Apply(fun, args) =>
        log(s"checking apply of ${fun.symbol.name}")
        if (fun.symbol.isSetter && args.head.symbol != null) {
          val argTpe = args.head.symbol.tpe // check type of (first) arg
          if (stackConfined.exists(t => argTpe.finalResultType <:< t)) {
            reporter.error(args.head.pos, s"${args.head} assigned to field, but ${argTpe.finalResultType} confined to stack")
          }
        }
        traverse(fun)
        args.foreach(traverse)

      case Try(block, catches, finalizer) =>
        super.traverse(block)
        catches.foreach(super.traverse)
        super.traverse(finalizer)

        log(s"analyzing try:")
        log(s"raw:\n${showRaw(tree)}")

        catches.foreach { case casedef @ CaseDef(pat, guard, body) =>
          body match {
            case Block(stats, expr) =>
              stats(1) match {
                case LabelDef(n, l, If(cond, thenp, elsep)) =>
                  cond match {
                    case TypeApply(left, List(right)) =>
                      // catching a subtype of `ControlThrowable`
                      if (right.tpe <:< ctrlThrowableTpe) {
                        thenp match {
                          case Apply(_, List(thenbody)) =>
                            checkPropagation(casedef, thenbody, right.tpe)
                          case Block(_, Apply(_, List(thenbody))) =>
                            checkPropagation(casedef, thenbody, right.tpe)
                        }
                      }
                  }
                case otherwise => log(s"no labeldef here")
              }
            case other => log(s"not a block here")
          }
        }

      case other =>
        super.traverse(tree)
    }
  }

  class SCPhase(prev: Phase) extends StdPhase(prev) {
    override def apply(unit: CompilationUnit): Unit = {
      log("LaCasa stack confinement checking...")
      val sct = new SCTraverser(unit)
      sct.traverse(unit.body)
    }
  }

  override def newPhase(prev: Phase): StdPhase =
    new SCPhase(prev)
}
