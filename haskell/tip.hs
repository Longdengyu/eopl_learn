module MyTip where 

-- tip language: https://cs.au.dk/~amoeller/spa/spa.pdf

import Text.Parsec
import Data.Functor.Identity 
import Control.Monad (guard)
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.State 
import Control.Monad.Trans.Except  
import qualified Control.Monad.Trans.Writer as W 
import Control.Monad.Except  


-- AST 

type Id = String 
data Expr 
    = TipInt Int
    | TipId Id 
    | TipAdd Expr Expr 
    | TipSub Expr Expr 
    | TipMul Expr Expr 
    | TipDiv Expr Expr 
    | TipMoreThan Expr Expr 
    | TipEq Expr Expr 
    | TipGroup Expr 
    | TipInput
    deriving Show 


-- Parser 
type Parser = ParsecT String () Identity 


ident_ :: Parser String 
ident_ = do 
    x <- letter 
    xs <- many alphaNum 
    return (x:xs) 

thetoken p = p <* spaces

symb s = thetoken (string s) 

keyword s = do 
    name <- string s <* spaces
    return name 

ident :: Parser String
ident = do 
    id <- thetoken ident_
    guard (not (elem id ["input", "output", "if", "else", "while"] ))
    return id 

positiveNum :: Parser Int 
positiveNum = do
    num <- thetoken (many1 digit)
    return (read num::Int)

negativeNum :: Parser Int 
negativeNum = do 
    keyword "-"
    num <- positiveNum
    return (-num)

parseExpr :: Parser Expr 
parseExpr = (chainl1 factor ops)

group :: Parser Expr 
group = do 
    keyword "("
    expr <- parseExpr
    keyword ")"
    return expr

factor = try tipIntExpr
    <|> try tipIdExpr
    <|> try group
    <|> try inputExpr

tipIntExpr :: Parser Expr 
tipIntExpr = do 
    num <- (positiveNum <|> negativeNum)
    return (TipInt num)

tipIdExpr :: Parser Expr
tipIdExpr = do 
    name <- ident
    return $ TipId name

-- TODO: figout how to handle the operator previledge
-- Ref: https://hackage.haskell.org/package/parsec-3.1.15.0/docs/Text-Parsec-Combinator.html
ops :: Parser (Expr -> Expr -> Expr)
ops = subOps 
    <|> addOps
    <|> mulOps
    <|> divOps
    <|> moreThanOps
    <|> eqOps


addOps :: Parser (Expr -> Expr -> Expr)
addOps = do 
    keyword "+"
    return $ TipAdd

subOps :: Parser (Expr -> Expr -> Expr)
subOps = do 
    keyword "-"
    return $ TipSub

mulOps :: Parser (Expr -> Expr -> Expr)
mulOps = do 
    keyword "*"
    return $ TipMul

divOps :: Parser (Expr -> Expr -> Expr)
divOps = do 
    keyword "/"
    return $ TipDiv

moreThanOps :: Parser (Expr -> Expr -> Expr)
moreThanOps = do 
    keyword ">"
    return $ TipMoreThan

eqOps :: Parser (Expr -> Expr -> Expr)
eqOps = do 
    keyword "="
    return $ TipEq

inputExpr :: Parser Expr 
inputExpr = do 
    keyword "input"
    return $ TipInput


data Statement 
    = AssignStm Id Expr
    | OutputStm Expr 
    | CompStm Statement Statement
    | IfStm Expr Statement Statement 
    | WhileStm Expr Statement
    | EmptyStm 
    deriving Show

parseStm :: Parser Statement
-- parseStm = assignStm -- TODO
parseStm = (chainl1 factorStm comp) <|> emptyStm

assignStm :: Parser Statement
assignStm = do 
    name <- ident
    keyword "="
    expr <- parseExpr 
    keyword ";"
    return $ AssignStm name expr

outputStm :: Parser Statement 
outputStm = do 
    keyword "output"
    expr <- parseExpr
    keyword ";"
    return $ OutputStm expr 

ifStm :: Parser Statement 
ifStm = do 
    keyword "if"
    keyword "("
    predExpr <- parseExpr
    keyword ")"
    keyword "{"
    trueStm <- parseStm
    keyword "}"
    falseStm <- option EmptyStm elsePart
    return $ IfStm predExpr trueStm falseStm
    where 
    elsePart = do 
        keyword "else"
        keyword "{"
        falseStm <- parseStm
        keyword "}"
        return falseStm

whileStm :: Parser Statement 
whileStm = do 
    keyword "while"
    keyword "("
    predExpr <- parseExpr
    keyword ")"
    keyword "{"
    stm <- parseStm
    keyword "}"
    return $ WhileStm predExpr stm

factorStm :: Parser Statement 
factorStm 
    = try assignStm
    <|> try outputStm
    <|> try ifStm
    <|> try whileStm

emptyStm :: Parser Statement 
emptyStm = return $ EmptyStm

comp :: Parser (Statement -> Statement -> Statement)
comp = do 
    return $ CompStm

testParse s = do 
    -- parse parseExpr "test" s 
    parse parseStm "test" s 


test = do 
    testParse "a = b;b = c;"

-- TODO: figout how to parse arith