module Ulc.Parsing
  (runParsing
  ,psDefinitions
  )
  where

import Data.Void (Void)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Ulc.Core (Primitive (..), Literal (..), Term (..))

import Text.Megaparsec
  (Parsec
  ,(<|>)
  ,try
  ,some
  ,someTill
  ,oneOf
  ,optional
  ,single
  ,eof
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

runParsing :: Parsing a -> String -> Either String a
runParsing action source =
  case parse action "" source of
    Left errorBundle -> Left (errorBundlePretty errorBundle)
    Right result -> Right result

psSpace :: Parsing ()
psSpace =
  Lexer.space space1 spLineComment spBlockComment where
    spLineComment = Lexer.skipLineComment "//"
    spBlockComment = Lexer.skipBlockComment "/*" "*/"

psLexeme :: Parsing a -> Parsing a
psLexeme parser =
  Lexer.lexeme psSpace parser

psSymbol :: String -> Parsing String
psSymbol string =
  Lexer.symbol psSpace string

psName :: Parsing String
psName =
  psLexeme (some (try (oneOf validCharacters))) where
    validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

psLiteral :: Parsing Literal
psLiteral =
  psLexeme (try ltReal <|> ltInteger) where
    ltInteger = positive <|> negative where
      positive = LtInteger <$> (optional (single '+') *> Lexer.decimal)
      negative = LtInteger <$> (single '-' *> (negate <$> Lexer.decimal))
    ltReal = positive <|> negative where
      positive = LtReal <$> (optional (single '+') *> Lexer.float)
      negative = LtReal <$> (single '-' *> (negate <$> Lexer.float))

psPrimitive :: Parsing Primitive
psPrimitive =
  psLexeme (psSymbol "{" *> (integerSum <|> realSum) <* psSymbol "}") where
    integerSum =
      PrIntegerSum <$> (psSymbol "integer_sum" *> psClosed) <*> psClosed
    realSum =
      PrRealSum <$> (psSymbol "real_sum" *> psClosed) <*> psClosed

psClosed :: Parsing Term
psClosed =
  psLexeme (parens <|> literal <|> primitive <|> reference) where
    parens = psSymbol "(" *> psTerm ")"
    literal = TrLiteral <$> psLiteral
    primitive = TrPrimitive <$> psPrimitive
    reference = TrReference <$> psName

psApplication :: String -> Parsing Term
psApplication terminator = do
  terms <- someTill psClosed (psSymbol terminator)
  return $ case terms of
    [] -> error "empty application"
    closed : [] -> closed
    function : arguments -> foldl TrApplication function arguments

psFunction :: String -> Parsing Term
psFunction terminator =
  psLexeme (try function) <|> psApplication terminator where
    function = do
      name <- psName <* psSymbol "=>"
      body <- psFunction terminator
      return (TrAbstraction $ abstract name body)

psTerm :: String -> Parsing Term
psTerm =
  psFunction

psDefinition :: Parsing (String, Term)
psDefinition =
  (,) <$> (psName <* psSymbol "=") <*> psTerm ";"

psDefinitions :: Parsing [(String, Term)]
psDefinitions =
  psLexeme (some psDefinition <* eof)
