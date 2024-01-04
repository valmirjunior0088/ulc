package ulc

import ulc.deBruijn.Envnt
import cats.parse.Parser

object Parse:
  def wspace = Parser.charIn(" \t\r\n").rep0.void
  def symbol(string: String) = Parser.string(string) <* wspace

  def number = Parser.charIn('0' to '9')
  def letter = Parser.charIn('a' to 'z') | Parser.charIn('A' to 'Z')
  def ident = (letter ~ (number | letter).rep0).string <* wspace

  def parens(envnt: Envnt[String]) =
    symbol("(") *> exprn(envnt) <* symbol(")")

  def varbl(envnt: Envnt[String]) =
    for ident <- ident
    yield Exprn.Var(envnt.indexOf(ident).get)

  def atomic(envnt: Envnt[String]) =
    parens(envnt) | varbl(envnt)

  def applc(envnt: Envnt[String]) =
    for exprs <- atomic(envnt).rep
    yield exprs.reduceLeft(Exprn.App(_, _))

  def scope(envnt: Envnt[String], ident: String) =
    for exprn <- exprn(ident +: envnt)
    yield Exprn.Scope(exprn)(ident)

  def abstr(envnt: Envnt[String]) =
    for
      ident <- ident <* symbol("=>")
      scope <- scope(envnt, ident)
    yield Exprn.Abs(scope)

  def defnt(envnt: Envnt[String]) =
    for
      ident <- symbol("let") *> ident
      exprn <- symbol("=") *> exprn(envnt)
      scope <- symbol(";") *> scope(envnt, ident)
    yield Exprn.Let(exprn, scope)

  def exprn(envnt: Envnt[String]): Parser[Exprn] = Parser.defer:
    defnt(envnt).backtrack | abstr(envnt).backtrack | applc(envnt)

  def program =
    Parser.start *> wspace *> exprn(Envnt.initial) <* Parser.end

  def apply(input: String) = program.parse(input) match
    case Right((_, output)) => Right(output)
    case Left(error) => Left(error.toString)
