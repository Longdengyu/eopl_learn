module MyExplicitRef where 

import Text.Parsec
import Data.Functor.Identity 
import Control.Monad (guard)
import Prelude hiding (lookup)
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.State 


-- AST 

type Id = String 
data Expr 
    = Const Int 
    | Let Id Expr Expr 
    | Var Id 
    | If Expr Expr Expr 
    | Diff Expr Expr 
    | Zero Expr 
    | Proc Id Expr 
    | Call Expr Expr
    | LetRec Id Id Expr Expr  
    | NewRef Expr 
    | DeRef Expr 
    | SetRef Expr Expr 
    | Begin [Expr] 
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
    guard (not (elem id ["let", "in", "if", "then", "else", "proc", "zero", "letrec", "newref","deref", "setref", "begin", "end"] ))
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

constExpr :: Parser Expr 
constExpr = do 
    num <- (positiveNum <|> negativeNum)
    return (Const num)

varExpr = Var <$> ident 
letExpr = do 
    keyword "let"
    name <- ident 
    keyword "="
    expr1 <- parseExpr 
    keyword "in"
    expr2 <- parseExpr 
    return (Let name expr1 expr2)

ifExpr = do 
    keyword "if" 
    expr1 <- parseExpr
    keyword "then"
    expr2 <- parseExpr 
    keyword "else"
    expr3 <- parseExpr 
    return (If expr1 expr2 expr3)

diffExpr = do 
    keyword "-"
    keyword "("
    expr1 <- parseExpr 
    keyword ","
    expr2 <- parseExpr 
    keyword ")"
    return (Diff expr1 expr2)

zeroExpr = do 
    keyword "zero?"
    keyword "("
    expr1 <- parseExpr 
    keyword ")"
    return (Zero expr1)

procExpr = do 
    keyword "proc"
    keyword "("
    id <- ident 
    keyword ")"
    body <- parseExpr 
    return (Proc id body)

callExpr = do 
    keyword "("
    rator <- parseExpr 
    rand <- parseExpr 
    keyword ")"
    return (Call rator rand)

letrecExpr = do 
    keyword "letrec"
    pName <- ident 
    bVar <- keyword "(" *> ident <* keyword ")"
    keyword "="
    pBody <- parseExpr
    keyword "in"
    letrecBody <- parseExpr 
    return (LetRec pName bVar pBody letrecBody)

newrefExpr = do 
    keyword "newref"
    expr1 <- (keyword "(" *> parseExpr <* keyword ")")
    return (NewRef expr1)

derefExpr = do 
    keyword "deref"
    expr1 <- keyword "(" *> parseExpr <* keyword ")"
    return (DeRef expr1)

setrefExpr = do 
    keyword "setref"
    keyword "("
    expr1 <- parseExpr 
    expr2 <- parseExpr
    keyword ")"
    return (SetRef expr1 expr2)

beginExpr = do 
    keyword "begin"
    exprs <- sepBy1 parseExpr (keyword ";")
    keyword "end"
    return (Begin exprs)

parseExpr :: Parser Expr 
parseExpr 
    =   try constExpr
    <|> try varExpr 
    <|> try letExpr
    <|> try zeroExpr 
    <|> try ifExpr 
    <|> try diffExpr
    <|> try procExpr
    <|> try callExpr
    <|> try letrecExpr
    <|> try newrefExpr
    <|> try derefExpr
    <|> try setrefExpr
    <|> try beginExpr

-- Interpreter 
data Procedure = Procedure Id Expr Env deriving Show 
data Value 
    = NumValue Int 
    | BoolValue Bool 
    | ProcValue Procedure 
    | RefValue Int 
    | UnitValue 
    deriving Show 

data Env 
    = EmptyEnv 
    | ExtendEnv Id Value Env 
    | ExtendEnvRec Id Id Expr Env 
    deriving Show 

-- type InterpM = MaybeT Identity 
type Store = [Value]
type InterpM = StateT Store (MaybeT Identity)

-- runInterp = runIdentity . runMaybeT
runInterp m = (runIdentity . runMaybeT . runStateT m) []

lookup :: Id -> Env -> InterpM Value 
lookup _ EmptyEnv = fail "lookup failed" 
lookup var (ExtendEnv name value oldEnv) = do 
    if var == name 
        then return value 
        else lookup var oldEnv
lookup var (ExtendEnvRec pName bVar pBody oldEnv) = do 
    if var == pName
        then return (ProcValue $ Procedure bVar pBody (ExtendEnvRec pName bVar pBody oldEnv))
        else lookup var oldEnv 

valueOf :: Expr -> Env -> InterpM Value 
valueOf (Const n) _ = return (NumValue n)
valueOf (Var name) env = lookup name env
valueOf (Zero expr1) env = do 
    NumValue n <- valueOf expr1 env 
    return (BoolValue (n == 0))
valueOf (Let var bindExpr bodyExpr) env = do 
    bindValue <- valueOf bindExpr env 
    valueOf bodyExpr (ExtendEnv var bindValue env)
valueOf (If expr1 expr2 expr3) env = do 
    BoolValue b <- valueOf expr1 env 
    if b 
        then valueOf expr2 env 
        else valueOf expr3 env 
valueOf (Diff expr1 expr2) env = do 
    NumValue num1 <- valueOf expr1 env 
    NumValue num2 <- valueOf expr2 env 
    return (NumValue (num1 - num2))
valueOf (Proc var body) env = return (ProcValue (Procedure var body env))
valueOf (Call rator rand) env = do 
    ProcValue (Procedure bindVar body pEnv) <- valueOf rator env 
    bindValue <- valueOf rand env 
    valueOf body (ExtendEnv bindVar bindValue pEnv)
valueOf (LetRec pName bVar pBody letrecBody) env = valueOf letrecBody  (ExtendEnvRec pName bVar pBody env)
valueOf (Begin exprs) env = case exprs of 
    x:[] -> valueOf x env
    x:xs -> do 
        valueOf x env 
        valueOf (Begin xs) env 
valueOf (NewRef expr1) env = do 
    value <- valueOf expr1 env 
    store <- get 
    let nextRef = length store 
    put (store ++ [value])
    return (RefValue nextRef)
valueOf (DeRef expr1) env = do 
    RefValue index <- valueOf expr1 env 
    store <- get 
    return (store !! index)
valueOf (SetRef expr1 expr2) env = do 
    RefValue index <- valueOf expr1 env 
    value <- valueOf expr2 env 
    store <- get 
    put (replaceNth index value store)
    return UnitValue
    where 
        replaceNth :: Int -> a -> [a] -> [a]
        replaceNth _ _ [] = []
        replaceNth n newVal (x:xs)
            | n == 0 = newVal:xs
            | otherwise = x:replaceNth (n-1) newVal xs

-- test 

testInterp s = do 
    case (parse parseExpr "testInterp" s) of 
        Right expr ->  runInterp (valueOf expr EmptyEnv)
        -- Left err -> err

testParse s = do 
    parse parseExpr "test" s 


test = do 
    testInterp "let a = newref (123) in begin setref (a 999); setref (a 100); deref (a) end"
 

-- TODO: know how to fix this problem 
-- TODO: using test framework to test the code 
-- ref: https://stackoverflow.com/questions/33057481/why-does-it-seem-that-the-parsec-choice-operator-depends-on-order-of-the-parsers
-- parsec sacrifices perfect nondeterminism