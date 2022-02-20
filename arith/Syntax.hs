module Syntax where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving(Show)


isnumericval :: Term -> Bool
isnumericval t = case t of
  TmZero -> True
  TmSucc t1 -> isnumericval t1
  _ -> False

--pred が残るとFalse→そういう規約

isval :: Term -> Bool
isval t = case t of
  TmTrue -> True
  TmFalse -> True
  t -> isnumericval t

