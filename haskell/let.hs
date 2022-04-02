module MyLet where 

-- import Text.ParserCombinators.Parsec 
import Text.Parsec
import Data.Functor.Identity 
import Control.Monad (guard)
import Prelude hiding (lookup)
import Control.Monad.Trans.Maybe 


-- AST 

type Id = String 
data Expr = Const Int 
    | Let Id Expr Expr 
    | Var Id 
    | If Expr Expr Expr 
    | Diff Expr Expr 
    | Zero Expr 
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
    guard (not (elem id ["let", "in", "if", "then", "else"] ))
    return id 

constExpr :: Parser Expr 
constExpr = do 
    num <- thetoken (many1 digit)
    return (Const (read num::Int))

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

parseExpr :: Parser Expr 
parseExpr = try constExpr 
    <|> try zeroExpr 
    <|> try diffExpr 
    <|> try ifExpr 
    <|> try letExpr
    <|> try varExpr


-- Interpreter 

data Value = NumValue Int | BoolValue Bool deriving Show 
data Env = EmptyEnv | ExtendEnv Id Value Env 

type InterpM = MaybeT Identity 

runInterp = runIdentity . runMaybeT

lookup :: Id -> Env -> InterpM Value 
lookup _ EmptyEnv = fail "lookup failed" 
lookup var (ExtendEnv name value oldenv) = do 
    if var == name then return value else lookup var oldenv

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
    if b then valueOf expr2 env else valueOf expr3 env 
valueOf (Diff expr1 expr2) env = do 
    NumValue num1 <- valueOf expr1 env 
    NumValue num2 <- valueOf expr2 env 
    return (NumValue (num1 - num2))


-- test 

testInterp s = do 
    case (parse parseExpr "testInterp" s) of 
        Right expr ->  runInterp (valueOf expr EmptyEnv)


test = do 
    -- parse parseExpr "test" "123"
    -- parse parseExpr "test" "abc"
    -- parse parseExpr "test" "let a = b in a"
    -- parse parseExpr "test" "if 123 then let a = b in c else if a then 2 else 3"
    -- parse parseExpr "test" "-(let a = b in c ,2)"
    -- parse parseExpr "test" "-(zero?(1),2)"
    -- testInterp "a"
    -- testInterp "zero?(zero?(1))"
    -- testInterp "if 1 then 1 else 2"
    testInterp "-(10, 3)"

-- TODO: know how to fix this problem 
-- TODO: using test framework to test the code 
-- ref: https://stackoverflow.com/questions/33057481/why-does-it-seem-that-the-parsec-choice-operator-depends-on-order-of-the-parsers
-- parsec sacrifices perfect nondeterminism