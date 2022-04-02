module MyProc where 

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
    | Proc Id Expr 
    | Call Expr Expr 
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
    guard (not (elem id ["let", "in", "if", "then", "else", "proc", "zero"] ))
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


parseExpr :: Parser Expr 
parseExpr = try constExpr
    <|> try varExpr 
    <|> try letExpr
    <|> try zeroExpr 
    <|> try ifExpr 
    <|> try diffExpr
    <|> try procExpr
    <|> callExpr

-- Interpreter 
data Procedure = Procedure Id Expr Env deriving Show 
data Value = NumValue Int 
    | BoolValue Bool 
    | ProcValue Procedure 
    deriving Show 
data Env = EmptyEnv | ExtendEnv Id Value Env deriving Show 

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
valueOf (Proc var body) env = return (ProcValue (Procedure var body env))
valueOf (Call rator rand) env = do 
    ProcValue (Procedure bindVar body pEnv) <- valueOf rator env 
    bindValue <- valueOf rand env 
    valueOf body (ExtendEnv bindVar bindValue pEnv)

-- test 

testInterp s = do 
    case (parse parseExpr "testInterp" s) of 
        Right expr ->  runInterp (valueOf expr EmptyEnv)
        -- Left err -> err


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
    -- testInterp "-(10, 3)"
    -- parse parseExpr "test" "proc (a) proc (b) -(a, b)"
    -- parse parseExpr "test" "(proc (a) zero?(a) b)"
    -- parse parseExpr "test" "(proc (a) a 1)"
    -- testInterp "(proc (a) -(a, 3) 10)"
    testInterp "let x = 200 in let f = proc (z) -(z,x) in let x = 100 in let g = proc (z) -(z,x) in -((f 1), (g 1))"
    -- parse parseExpr "test" "l" -- TODO: fix this: using try
 

-- TODO: know how to fix this problem 
-- TODO: using test framework to test the code 
-- ref: https://stackoverflow.com/questions/33057481/why-does-it-seem-that-the-parsec-choice-operator-depends-on-order-of-the-parsers
-- parsec sacrifices perfect nondeterminism