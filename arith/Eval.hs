module Eval where

import Control.Monad

import Syntax

eval1 :: Term -> Maybe Term
eval1 t = case t of 
    (TmIf TmTrue t2 _) -> return t2
    (TmIf TmFalse _ t3) -> return t3
    (TmIf t1 t2 t3) -> liftM3 TmIf (eval1 t1) (return t2) (return t3)
    (TmSucc t) -> fmap TmSucc (eval1 t)
    (TmPred (TmSucc t)) -> if isnumericval t then return t else Nothing 
    (TmPred TmZero) -> return TmZero
    (TmPred t) -> fmap TmPred (eval1 t)
    (TmIsZero TmZero) ->  return TmTrue
    (TmIsZero (TmSucc t)) -> if isnumericval t then return TmFalse else Nothing 
    (TmIsZero t) -> fmap TmIsZero (eval1 t)
    
    TmTrue -> Nothing 
    TmFalse -> Nothing 
    TmZero -> Nothing

-- pred使うとnvの定義上直感的にアレな結果になりそう

eval :: Term -> Term
eval t =
  maybe t eval (eval1 t)


-- 4.2.2.

