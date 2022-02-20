module Syntax where
import Context

data Term = TmVar Int Int
          | TmAbs String Term
          | TmApp Term Term
          deriving(Show)

printTm :: Context -> Term -> String 
printTm ctx t = case t of 
        TmAbs x t1 ->
                let (ctx', x') = pickFreshName ctx x in
                        "(lambda " ++ x' ++ "." ++ printTm ctx' t1 ++ ")"
        TmApp t1 t2 ->
                "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
        TmVar x n -> getIndexName n ctx