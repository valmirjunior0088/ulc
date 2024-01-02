package ulc

import ulc.deBruijn.Envnt
import ulc.deBruijn.Index

enum Exprn:
  case Var(index: Index)
  case Abs(scope: Exprn.Scope)
  case App(funct: Exprn, argmt: Exprn)
  case Let(exprn: Exprn, scope: Exprn.Scope)

  def eval(envnt: Envnt[Value]): Value = this match
    case Var(index) => envnt(index).get
    case Abs(scope) => Exprn.evalAbs(envnt, scope)
    case App(funct, argmt) => Exprn.evalApp(envnt, funct, argmt)
    case Let(exprn, scope) => Exprn.evalLet(envnt, exprn, scope)

  def eval: Value = eval(Envnt.initial)

object Exprn:
  final case class Scope(val body: Exprn)(val name: String):
    def eval(envnt: Envnt[Value])(value: Value) = body.eval(value +: envnt)

  private def evalAbs(envnt: Envnt[Value], scope: Scope) =
    Value.abs(scope.name, scope.eval(envnt))

  private def evalApp(envnt: Envnt[Value], funct: Exprn, argmt: Exprn) =
    Value.app(funct.eval(envnt), argmt.eval(envnt))

  private def evalLet(envnt: Envnt[Value], exprn: Exprn, scope: Scope) =
    scope.eval(envnt)(exprn.eval(envnt))
