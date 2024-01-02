package ulc

import ulc.deBruijn.Depth
import ulc.deBruijn.Level
import ulc.deBruijn.Spine

enum Value:
  private case Var(level: Level)
  private case Abs(scope: Value.Scope)
  private case App(level: Level, spine: Spine[Value])

  def quote(depth: Depth): Exprn = this match
    case Var(level) => Value.quoteVar(depth, level)
    case Abs(scope) => Value.quoteAbs(depth, scope)
    case App(level, spine) => Value.quoteApp(depth, level, spine)

  def quote: Exprn = quote(Depth.initial)

object Value:
  private final case class Scope(val body: Value => Value)(val name: String):
    def apply(value: Value) = body(value)

  def abs(name: String, body: Value => Value) = Abs(Scope(body)(name))

  def app(funct: Value, argmt: Value) = funct match
    case Var(level) => App(level, Spine(argmt))
    case Abs(scope) => scope(argmt)
    case App(level, spine) => App(level, spine :+ argmt)

  private def quoteVar(depth: Depth, level: Level) =
    Exprn.Var(level.quote(depth))

  private def quoteScope(depth: Depth, scope: Scope) =
    Exprn.Scope(scope(Var(depth.level)).quote(depth.descend))(scope.name)

  private def quoteAbs(depth: Depth, scope: Scope) =
    Exprn.Abs(quoteScope(depth, scope))

  private def quoteAppArgmt(depth: Depth)(funct: Exprn, argmt: Value) =
    Exprn.App(funct, argmt.quote(depth))

  private def quoteApp(depth: Depth, level: Level, spine: Spine[Value]) =
    spine(Exprn.Var(level.quote(depth)), quoteAppArgmt(depth))
