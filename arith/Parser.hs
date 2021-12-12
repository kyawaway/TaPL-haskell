module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax

reservedWord =
    emptyDef { Token.reservedNames =
        ["if"
        ,"then"
        ,"else"
        ,"true"
        ,"false"
        ,"succ"
        ,"pred"
        ,"0"
        ,"iszero"
    ]}

lexer = Token.makeTokenParser reservedWord
reserved = Token.reserved lexer
parens = Token.parens lexer
whiteWSpace = Token.whiteSpace lexer


parseTerm :: Parser Term
parseTerm = parens parseTerm
        <|> parseTrue
        <|> parseFalse
        <|> parseIf
        <|> parseZero
        <|> parseSucc
        <|> parsePred
        <|> parseIsZero


parseTrue :: Parser Term
parseTrue = reserved "true" >> return TmTrue

parseFalse :: Parser Term
parseFalse = reserved "false" >> return TmFalse

-- 後でApplicativeに...

parseIf :: Parser Term
parseIf =
     do reserved "if"
        t1 <- parseTerm
        reserved "then"
        t2 <- parseTerm
        reserved "else"
        t3 <- parseTerm
        return $ TmIf t1 t2 t3

parseZero :: Parser Term 
parseZero = reserved "0" >> return TmZero

parseSucc :: Parser Term
parseSucc =
    do reserved "succ"
       TmSucc <$> parseTerm


parsePred :: Parser Term
parsePred =
    do reserved "pred"
       TmPred <$> parseTerm


parseIsZero :: Parser Term 
parseIsZero =
    do reserved "iszero"
       TmIsZero <$> parseTerm



