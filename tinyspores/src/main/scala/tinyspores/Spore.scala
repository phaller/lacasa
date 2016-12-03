/**
 * Copyright (C) 2016 Philipp Haller
 */
package tinyspores

import scala.language.experimental.macros
import scala.language.implicitConversions


import scala.reflect.macros.blackbox.Context


/**
  * A small implementation of spores as introduced in the following paper:
  *
  * Heather Miller, Philipp Haller, and Martin Odersky:
  * Spores: A Type-Based Foundation for Closures in the Age of Concurrency
  * and Distribution. ECOOP 2014
  * Paper preprint: https://infoscience.epfl.ch/record/191239/files/spores_1.pdf
  *
  * The main differences compared to https://github.com/scalacenter/spores are:
  *
  * 1. Rather than starting from the form `spore { .. }` the starting point is
  *    an implicit conversion from `T => R` to `Spore[T, R]`. This allows taking
  *    into account constraints expressed in the expected type, including
  *    required type classes and excluded types.
  *    (Implicitly converting from a `Spore[T, R]` to a spore with constraints is
  *    brittle, since it involves a macro transforming code produced by a macro.)
  *
  * 2. Rather than generating a new spore subtype with a `Captured` type member,
  *    a different approach is taken to express constraints on captured types, with
  *    the advantage that a blackbox macro is sufficient.
  *
  */
object Spore {

  implicit def function2Spore[T, R, K, A](fun: T => R): Spore[T, R] { type CC = K; type Excluded = A } =
    macro function2SporeImpl[T, R, K, A]

  def function2SporeImpl[T, R, K, A](c: Context)(fun: c.Tree)
      (implicit ttag: c.WeakTypeTag[T], rtag: c.WeakTypeTag[R], ktag: c.WeakTypeTag[K], atag: c.WeakTypeTag[A]): c.Tree = {
    import c.universe._

    val atpe = weakTypeOf[A]
    val excludedTypes: List[Type] =
      if (atpe <:< weakTypeOf[(Any, Any)] ||
        atpe <:< weakTypeOf[(Any, Any, Any)] ||
        atpe <:< weakTypeOf[(Any, Any, Any, Any)] ||
        atpe <:< weakTypeOf[(Any, Any, Any, Any, Any)] ||
        atpe <:< weakTypeOf[(Any, Any, Any, Any, Any, Any)])
        atpe.typeArgs
      else List(atpe)

    object typeCollector extends Traverser {
      var mentionedTypes = List[TypeTree]()
      override def traverse(tree: Tree): Unit = tree match {
        case tt @ TypeTree() => mentionedTypes = tt :: mentionedTypes
        case _ => super.traverse(tree)
      }
    }
    typeCollector.traverse(fun)

    val NothingType = typeOf[Nothing]
    // Check that btm is indeed the bottom type and that tpe is not
    def isBottomType(btm: Type, tpe: Type) =
      btm =:= NothingType && !(tpe =:= btm)

    // abort if some TypeTree in `fun` has a type that is <:< of something in `atpe`
    typeCollector.mentionedTypes.foreach { t =>
      excludedTypes.foreach(at =>
        if (t.tpe <:< at && !isBottomType(t.tpe, at)) {
          c.echo(t.pos, s"Expression has type ${t.tpe}, but type $at is excluded")
          c.abort(t.pos, s"Expression has type ${t.tpe}, but type $at is excluded")
        }
      )}

    val optContextSym = ktag.tpe match {
      case ExistentialType(syms, TypeRef(_, sym, _)) =>
        Some(sym)
      case NothingType =>  // OK
        None
      case other =>
        c.echo(fun.pos, "Nothing or wildcard type expected for context bound of captured types")
        c.abort(fun.pos, "Nothing or wildcard type expected for context bound of captured types")
        None
    }

    /* Checks whether the owner chain of `sym` contains `owner`.
     *
     * @param sym   the symbol to be checked
     * @param owner the owner symbol that we try to find
     * @return      whether `owner` is a direct or indirect owner of `sym`
     */
    def ownerChainContains(sym: Symbol, owner: Symbol): Boolean =
      sym != null && (sym.owner == owner || {
        sym.owner != NoSymbol && ownerChainContains(sym.owner, owner)
      })

    def symbolNotCaptured(sym: Symbol): Boolean =
      sym.isStatic || ownerChainContains(sym, fun.symbol)

    val capturedTypes = fun match {
      case Function(vparams, body) =>
        // collect captured variables
        var captured: List[Symbol] = List()
        val traverser = new Traverser {
          override def traverse(tree: Tree): Unit = tree match {
            case id: Ident =>
              if (!symbolNotCaptured(id.symbol))
                captured = id.symbol :: captured
            case _ =>
              super.traverse(tree)
          }
        }
        traverser.traverse(body)
        captured.map(_.typeSignature)

      case _ =>
        c.echo(fun.pos, "function literal expected")
        c.abort(fun.pos, "function literal expected")
        List()
    }

    if (optContextSym.nonEmpty) {
      capturedTypes.foreach { ctpe =>
        val contextType = appliedType(optContextSym.get, ctpe)
        if (c.inferImplicitValue(contextType) == EmptyTree) {
          c.echo(fun.pos, s"no implicit value of type $contextType found")
          c.abort(fun.pos, s"no implicit value of type $contextType found")
        }
      }
    }

    q"""
      new Spore[$ttag, $rtag] {
        type CC = $ktag
        type Excluded = $atag
        def apply(x: $ttag): $rtag = $fun(x)
      }
    """
  }

}

trait Spore[-T, +R] extends (T => R) {

  type CC

  type Excluded

}
