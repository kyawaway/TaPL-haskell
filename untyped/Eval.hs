module Eval where

import Control.Monad

import Syntax

-- helper

termShift :: Int -> Term -> Term
termShift d = walk 0
    where walk c (TmVar x n)
            | x>=c      = TmVar (x+d) (n+d)
            | otherwise = TmVar x (n+d)
          walk c (TmAbs x t1) = TmAbs x (walk (c+1) t1)
          walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
    where walk c (TmVar x n)
            | x == j+c = s
            | otherwise = TmVar x n
          walk c (TmAbs x t1) = TmAbs x (walk (c+1) t1)
          walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

-- eval

isVal :: Term -> Bool
isVal (TmAbs _ _ ) = True
isVal _ = False


eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs _ t12) v2)
  | isVal v2 = return $ termSubstTop v2 t12
eval1 (TmApp t1 t2)
  | isVal t1 = fmap (TmApp t1) (eval1 t2)
  | otherwise = liftM2 TmApp (eval1 t1) (return t2)
eval1 _ = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
