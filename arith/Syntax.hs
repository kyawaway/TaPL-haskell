module Syntax where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving(Show)


-- 使ってない

isnumericval :: Term -> Bool
isnumericval t = case t of
  TmZero -> True
  TmSucc t1 -> isnumericval t1
  _ -> False


isval :: Term -> Bool
isval t = case t of
  TmTrue -> True
  TmFalse -> True
  t -> isnumericval t




