package lacasa

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.tools.nsc.transform._
import scala.collection.{mutable, immutable}

import Util._


/** ControlThrowable checker
  */
class ControlThrowableComponent(val plugin: Plugin) extends NscPluginComponent {
  val global: plugin.global.type = plugin.global
  import global.{log => _, _}
  import definitions._
  import reflect.internal.Flags._

  override val runsAfter = List("refchecks")
  val phaseName = "lacasa-controlthrowable"

  private val ctrlThrowableClass = rootMirror.getClassByName(newTermName("scala.util.control.ControlThrowable"))
  private val ctrlThrowableTpe = typeOf[scala.util.control.ControlThrowable]
  private val uncheckedCatchMethod = plugin.boxModule.moduleClass.tpe.member(newTermName("uncheckedCatchControl"))

  private class CTTraverser(unit: CompilationUnit) extends Traverser {
    var currentMethods: List[Symbol] = List()

    def checkPropagation(casedef: CaseDef, thenbody: Tree, rightTpe: Type): Unit = thenbody match {
      case Block((sel @ Select(_, _)) :: moreStats, theExpr) =>
        log(s"LLLLLLLLLLLLLLLL")
        log(s"LLLLLLLLLLLLLLLL")
        log(s"sel.symbol: ${sel.symbol}")
        log(s"symbol of uncheckedCatch: $uncheckedCatchMethod")
        if (sel.symbol == uncheckedCatchMethod) {
          // OK: escape hatch used
        } else {
          reporter.error(casedef.pos, s"caught ControlThrowable of type ${rightTpe} not propagated")
        }

      case Block(Throw(theExc) :: moreStats, theExpr) =>
        log(s"type of thrown exception: ${theExc.tpe}")
        if (theExc.tpe <:< rightTpe) {
          // OK if type of thrown exception subtype of caught exception
          log(s"LLLLLLLLLLLLLLLL")
          log(s"LLLLLLLLLLLLLLLL")
          log(s"right.tpe:  ${rightTpe}")
          log(s"theExc.tpe: ${theExc.tpe}")
        } else {
          reporter.error(casedef.pos, s"caught ControlThrowable of type ${rightTpe} not propagated")
        }

      case sel @ Select(_, _) =>
        if (sel.symbol == uncheckedCatchMethod) {
          // OK: escape hatch used
        } else {
          reporter.error(casedef.pos, s"caught ControlThrowable of type ${rightTpe} not propagated")
        }

      case other =>
        reporter.error(casedef.pos, s"caught ControlThrowable of type ${rightTpe} not propagated")
    }

    override def traverse(tree: Tree): Unit = tree match {
      case ClassDef(mods, name, tparams, impl) =>
        log(s"checking class $name")
        traverse(impl)

      case methodDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        log(s"checking method definition $name")
        currentMethods = methodDef.symbol :: currentMethods
        traverse(rhs)
        currentMethods = currentMethods.tail

      case Try(block, catches, finalizer) =>
        traverse(block) // bottom-up analysis

        log(s"AAAAAAAAAAAAAAAA")
        log(s"analyzing try:")
        log(s"raw:\n${showRaw(tree)}")
        catches.foreach { case casedef @ CaseDef(pat, guard, body) =>
          body match {
            case Block(stats, expr) =>
              stats(1) match {
                case LabelDef(n, l, If(cond, thenp, elsep)) =>
                  log(s"KKKKKKKKKKKKKKKK")
                  log(s"KKKKKKKKKKKKKKKK")
                  log(s"KKKKKKKKKKKKKKKK")
                  log(s"KKKKKKKKKKKKKKKK")
                  log(s"KKKKKKKKKKKKKKKK")
                  cond match {
                    case TypeApply(left, List(right)) =>
                      log(s"$right")
                      if (right.tpe <:< ctrlThrowableTpe) {
                        log(s"KKKKKKKKKKKKKKKK")
                        log(s"KKKKKKKKKKKKKKKK")
                        log(s"KKKKKKKKKKKKKKKK")
                        log(s"KKKKKKKKKKKKKKKK")
                        log(s"KKKKKKKKKKKKKKKK")
                        thenp match {
                          case Apply(_, List(thenbody)) =>
                            log(s"THE THEN PART (1):")
                            log(s"${showRaw(thenbody)}")
                            checkPropagation(casedef, thenbody, right.tpe)

                          case Block(_, Apply(_, List(thenbody))) =>
                            log(s"THE THEN PART (2):")
                            log(s"${showRaw(thenbody)}")
                            checkPropagation(casedef, thenbody, right.tpe)
                        }
                      }
                  }
                case otherwise => log(s"no labeldef here")
              }
            case other => log(s"not a block here")
          }
        }

      case _ =>
        super.traverse(tree)
    }
  }

  class CTPhase(prev: Phase) extends StdPhase(prev) {
    override def apply(unit: CompilationUnit): Unit = {
      log("LaCasa ControlThrowable checking...")
      try {
        (new CTTraverser(unit)).traverse(unit.body)
      } catch {
        case any: Throwable =>
          any.printStackTrace()
      }
    }
  }

  override def newPhase(prev: Phase): StdPhase =
    new CTPhase(prev)
}
