module Repl where

import System.IO
import Control.Monad
import Text.Parsec
import Syntax
import Parser
import Context
import Eval

repl :: IO ()
repl = 
    let ctx = newContext 
      in loop ctx
  where
    loop ctx = do putStr ">> "
                  hFlush stdout
                  s <- getLine
                  case runParser parseTerm ctx  "stdin" s of
                      Left err -> print err >> loop ctx
                      Right x -> do print $ (printTm ctx . eval) x
                                    loop ctx
