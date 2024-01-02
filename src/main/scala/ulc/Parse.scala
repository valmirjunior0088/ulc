package ulc

import fastparse.MultiLineWhitespace._
import fastparse._
import ulc.deBruijn.Envnt

object Parse:
  def parentheses[$: P](envnt: Envnt[String]) = P:
    "(" ~ expression(envnt) ~ ")"

  def identifier[$: P] = P:
    CharIn("a-zA-Z").rep(min = 1).!

  def scope[$: P](envnt: Envnt[String], ident: String) = P:
    for {
      exprn <- expression(ident +: envnt)
    } yield Exprn.Scope(exprn)(ident)

  def definition[$: P](envnt: Envnt[String]) = P:
    for {
      ident <- "let" ~ identifier
      exprn <- "=" ~ expression(envnt)
      scope <- ";" ~ scope(envnt, ident)
    } yield Exprn.Let(exprn, scope)

  def variable[$: P](envnt: Envnt[String]) = P:
    for {
      ident <- identifier
    } yield Exprn.Var(envnt.indexOf(ident).get)

  def closed[$: P](envnt: Envnt[String]) = P:
    parentheses(envnt) | variable(envnt)

  def application[$: P](envnt: Envnt[String]) = P:
    for {
      exprs <- closed(envnt).rep(min = 1)
    } yield exprs.reduceLeft(Exprn.App(_, _))

  def abstraction[$: P](envnt: Envnt[String]) = P:
    for {
      ident <- identifier ~ "=>"
      scope <- scope(envnt, ident)
    } yield Exprn.Abs(scope)

  def expression[$: P](envnt: Envnt[String]): P[Exprn] = P:
    definition(envnt) | abstraction(envnt) | application(envnt)

  def program[$: P] = P:
    Start ~ expression(Envnt.initial) ~ End

  def apply(input: String) = parse(input, program(_)) match
    case Parsed.Success(exprn, _) => Right(exprn)
    case Parsed.Failure(_, _, extra) => Left(extra.trace().longMsg)
