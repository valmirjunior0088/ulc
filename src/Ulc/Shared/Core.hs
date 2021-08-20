{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ulc.Shared.Core
  (Literal (..)
  ,Primitive (..)
  ,Variable (..)
  ,Term (..)
  ,Item (..)
  )
  where

data Literal =
  LtInteger Int |
  LtReal Float
  deriving (Show)

data Primitive =
  PrIntegerSum Term Term |
  PrRealSum Term Term
  deriving (Show)

newtype Variable =
  Variable Int
  deriving (Show, Num, Enum, Eq)

data Term =
  TrLiteral Literal |
  TrPrimitive Primitive |
  TrReference String |
  TrVariable Variable |
  TrAbstraction Term |
  TrApplication Term Term
  deriving (Show)

data Item =
  Item String Term
