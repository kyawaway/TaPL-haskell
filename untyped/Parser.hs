module Parser where

import Control.Monad
import Data.Functor.Identity
import Text.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List

import Syntax
import Context

type Parser a = ParsecT String Context Identity a

reservedWord =
    emptyDef { Token.reservedNames =
        ["lambda"
    ]}

lexer = Token.makeTokenParser reservedWord
reserved = Token.reserved lexer
identifier = Token.identifier lexer
parens = Token.parens lexer
dot = Token.dot lexer
whiteSpace = Token.whiteSpace lexer


getVarIndex :: (Monad m, Eq a) => a -> [a] -> m Int
getVarIndex var ctx =
  case elemIndex var ctx of
    Just i  -> return i
    Nothing -> error "Unbound variable name"

parseVar :: Parser Term
parseVar =
  do var <- identifier
     ctx <- getState
     idx <- getVarIndex var ctx
     return $ TmVar idx (length ctx)

parseAbs :: Parser Term
parseAbs =
  do reserved "lambda"
     var  <- identifier
     dot
     ctx  <- getState
     setState $ bindName var ctx
     term <- parseTerm
     setState ctx
     return $ TmAbs var term

parseTerm :: Parser Term
parseTerm = 
  chainl1 (parseAbs <|> parseVar <|> parens parseTerm) 
          (return TmApp) 