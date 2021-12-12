module Repl where 

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Parser
import Eval        

repl :: IO ()
repl = do putStr ">> "
          hFlush stdout
          s <- getLine
          case parse parseTerm "stdin" s of
            Right x -> do print (eval x)
                          repl
            Left err -> print err