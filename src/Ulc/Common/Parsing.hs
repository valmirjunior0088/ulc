module Ulc.Common.Parsing
  ( parse
  )
  where

import Data.Void (Void)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Ulc.Common.Core
  ( Primitive (..)
  , Literal (..)
  , Term (..)
  , Item (..)
  )

import Text.Megaparsec
  ( Parsec
  , (<|>)
  , try
  , some
  , someTill
  , oneOf
  , optional
  , single
  , eof
  )

abstract :: String -> Term -> Term
abstract name =
  go 0 where
    go depth term =
      case term of
        TrLiteral literal ->
          TrLiteral literal
        TrPrimitive (PrIntegerSum left right) ->
          TrPrimitive (PrIntegerSum (go depth left) (go depth right))
        TrPrimitive (PrRealSum left right) ->
          TrPrimitive (PrRealSum (go depth left) (go depth right))
        TrReference reference | name == reference ->
          TrVariable depth
        TrReference reference ->
          TrReference reference
        TrVariable variable ->
          TrVariable variable
        TrAbstraction scope ->
          TrAbstraction (go (succ depth) scope)
        TrApplication function argument ->
          TrApplication (go depth function) (go depth argument)

type Parsing a =
  Parsec Void String a

parseSpace :: Parsing ()
parseSpace =
  Lexer.space space1 spLineComment spBlockComment where
    spLineComment = Lexer.skipLineComment "//"
    spBlockComment = Lexer.skipBlockComment "/*" "*/"

parseLexeme :: Parsing a -> Parsing a
parseLexeme parser =
  Lexer.lexeme parseSpace parser

parseSymbol :: String -> Parsing String
parseSymbol string =
  Lexer.symbol parseSpace string

parseName :: Parsing String
parseName =
  parseLexeme (some (try (oneOf validCharacters))) where
    validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

parseLiteral :: Parsing Literal
parseLiteral =
  parseLexeme (try ltReal <|> ltInteger) where
    ltInteger = positive <|> negative where
      positive = LtInteger <$> (optional (single '+') *> Lexer.decimal)
      negative = LtInteger <$> (single '-' *> (negate <$> Lexer.decimal))
    ltReal = positive <|> negative where
      positive = LtReal <$> (optional (single '+') *> Lexer.float)
      negative = LtReal <$> (single '-' *> (negate <$> Lexer.float))

parsePrimitive :: Parsing Primitive
parsePrimitive =
  parseLexeme (parseSymbol "{" *> primitive <* parseSymbol "}") where
    prIntegerSum =
      PrIntegerSum
        <$> (parseSymbol "integer_sum" *> parseClosed)
        <*> parseClosed
    prRealSum =
      PrRealSum
        <$> (parseSymbol "real_sum" *> parseClosed)
        <*> parseClosed
    primitive =
      prIntegerSum <|> prRealSum

parseClosed :: Parsing Term
parseClosed =
  parseLexeme (trParens <|> trLiteral <|> trPrimitive <|> trReference) where
    trParens = parseSymbol "(" *> parseTerm ")"
    trLiteral = TrLiteral <$> parseLiteral
    trPrimitive = TrPrimitive <$> parsePrimitive
    trReference = TrReference <$> parseName

parseApplication :: String -> Parsing Term
parseApplication terminator = do
  terms <- someTill parseClosed (parseSymbol terminator)

  return $ case terms of
    [] -> error "empty application"
    closed : [] -> closed
    function : arguments -> foldl TrApplication function arguments

parseFunction :: String -> Parsing Term
parseFunction terminator =
  parseLexeme (try trFunction) <|> parseApplication terminator where
    trFunction = do
      name <- parseName <* parseSymbol "=>"
      body <- parseFunction terminator
      return (TrAbstraction $ abstract name body)

parseTerm :: String -> Parsing Term
parseTerm =
  parseFunction

parseItem :: Parsing Item
parseItem =
  Item <$> (parseName <* parseSymbol "=") <*> parseTerm ";"

parseItems :: Parsing [Item]
parseItems =
  parseLexeme (some parseItem <* eof)

parse :: String -> Either String [Item]
parse source =
  case Megaparsec.parse parseItems "" source of
    Left errorBundle -> Left (Megaparsec.errorBundlePretty errorBundle)
    Right items -> Right items
