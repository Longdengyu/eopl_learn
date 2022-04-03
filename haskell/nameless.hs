module MyNameLess where 

import Text.Parsec
import Data.Functor.Identity 
import Control.Monad (guard)
import Prelude hiding (lookup)
import Control.Monad.Trans.Maybe 


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
    -- | LetRec Id Id Expr Expr  
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
    guard (not (elem id ["let", "in", "if", "then", "else", "proc", "zero", "letrec"] ))
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
constExpr = Const <$> (positiveNum <|> negativeNum)

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

-- letrecExpr = do 
--     keyword "letrec"
--     pName <- ident 
--     bVar <- keyword "(" *> ident <* keyword ")"
--     keyword "="
--     pBody <- parseExpr
--     keyword "in"
--     letrecBody <- parseExpr 
--     return (LetRec pName bVar pBody letrecBody)


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
    -- <|> try letrecExpr


-- Interpreter 
data Procedure = Procedure Id Expr Env deriving Show 
data Value 
    = NumValue Int 
    | BoolValue Bool 
    | ProcValue Procedure 
    | NLProcValue NLProcedure 
    deriving Show 

data NLProcedure = NLProcedure NLExpr [Value] deriving Show 

data Env 
    = EmptyEnv 
    | ExtendEnv Id Value Env 
    | ExtendEnvRec Id Id Expr Env 
    deriving Show 

type InterpM = MaybeT Identity 

runInterp = runIdentity . runMaybeT

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
-- valueOf (LetRec pName bVar pBody letrecBody) env = valueOf letrecBody  (ExtendEnvRec pName bVar pBody env)


-- nameless translator 
data SEnv
    = EmptySEnv 
    | ExtendSEnv String SEnv
    deriving Show 


sApply :: SEnv -> String -> Int 
sApply EmptySEnv _ = error "not found"
sApply (ExtendSEnv name oldSEnv) var = do 
    if var == name
        then 0
        else 1 + (sApply oldSEnv var) 

data NLExpr 
    = NLConst Int 
    | NLAt Int
    | NLLet NLExpr NLExpr
    | NLIf NLExpr NLExpr NLExpr 
    | NLDiff NLExpr NLExpr 
    | NLZero NLExpr 
    | NLProc NLExpr 
    | NLCall NLExpr NLExpr
    -- | NLLetRec NLExpr NLExpr  
    deriving Show 

translate :: Expr -> SEnv -> NLExpr
translate (Const n) _ = NLConst n 
translate (Var var) sEnv = NLAt $ sApply sEnv var 
translate (Let name bindExpr bodyExpr) sEnv = do 
    let nlBindExpr = translate bindExpr bindSEnv
    let nlBodyExpr = translate bodyExpr bodySEnv
    NLLet nlBindExpr nlBodyExpr
    where 
        bindSEnv = sEnv 
        bodySEnv = ExtendSEnv name sEnv
translate (If expr1 expr2 expr3) sEnv = do 
    let nlExpr1 = translate expr1 sEnv 
    let nlExpr2 = translate expr2 sEnv 
    let nlExpr3 = translate expr3 sEnv
    NLIf nlExpr1 nlExpr2 nlExpr3 
translate (Diff expr1 expr2) sEnv = do 
    let nlExpr1 = translate expr1 sEnv 
    let nlExpr2 = translate expr2 sEnv 
    NLDiff nlExpr1 nlExpr2 
translate (Zero expr1) sEnv = do 
    let nlExpr1 = translate expr1 sEnv 
    NLZero nlExpr1 
translate (Proc name bodyExpr) sEnv = do 
    let nlBody = translate bodyExpr bodySenv
    NLProc nlBody
    where 
        -- bodySenv = ExtendSEnv name 1 (incSEnv sEnv)
        bodySenv = ExtendSEnv name sEnv
translate (Call expr1 expr2) sEnv = do 
    let nlExpr1 = translate expr1 sEnv 
    let nlExpr2 = translate expr2 sEnv 
    NLCall nlExpr1 nlExpr2 
-- translate (LetRec pName bVar pBody letrecBody) sEnv = do 
--     let nlProcBody = translate pBody pBodySEnv
--     let nlLetRecBody = translate letrecBody letrecBodySEnv
--     NLLetRec nlProcBody nlLetRecBody
--     where 
--         pBodySEnv = ExtendSEnv bVar (ExtendSEnv pName  sEnv)
--         letrecBodySEnv = ExtendSEnv pName sEnv



nlValueOf :: NLExpr -> [Value] -> InterpM Value 
nlValueOf (NLConst n) _ = return $ NumValue n
nlValueOf (NLAt index) a = return $ a !! index -- NOTE: array index start from 0 not 1
nlValueOf (NLLet bindExpr bodyExpr) a = do 
    bindValue <- nlValueOf bindExpr a 
    nlValueOf bodyExpr (bindValue:a)
nlValueOf (NLIf expr1 expr2 expr3) a = do 
    BoolValue b <- nlValueOf expr1 a
    if b
        then nlValueOf expr2 a 
        else nlValueOf expr3 a 
nlValueOf (NLDiff expr1 expr2) a = do 
    NumValue num1 <- nlValueOf expr1 a 
    NumValue num2 <- nlValueOf expr2 a 
    return (NumValue (num1 - num2))
nlValueOf (NLZero expr1) a = do 
    NumValue num1 <- nlValueOf expr1 a 
    return $ BoolValue (num1 == 0)
nlValueOf (NLProc expr1) a = do 
    return $ NLProcValue $ NLProcedure expr1 a 
nlValueOf (NLCall callerExpr calleeExpr) a = do 
    NLProcValue (NLProcedure expr callerEnv) <- nlValueOf callerExpr a 
    bindValue <- nlValueOf calleeExpr a 
    nlValueOf expr (bindValue:a)
-- nlValueOf (NLLetRec pBodyExpr letRecBodyExpr) a = do
--     nlValueOf letRecBodyExpr someEnv
--     where 
--         someEnv = (NLProcValue (NLProcedure pBodyExpr a)):a

-- test 

testInterp s = do 
    case (parse parseExpr "testInterp" s) of 
        Right expr ->  runInterp (valueOf expr EmptyEnv)
        -- Left err -> err

testTranslate s = do 
    case parse parseExpr "test" s of 
        Right expr -> do 
            let nlExpr = translate expr EmptySEnv
            nlExpr

testInterpNl s = do 
    case parse parseExpr "test" s of 
        Right expr -> do 
            let nlExpr = translate expr EmptySEnv
            nlValueOf nlExpr []

test = do 
    -- testInterp "letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 10)" -- TODO: fix this: using try
    -- testInterpNl "letrec p(x) = x in (p 1)" 
    testInterpNl "let x = proc (x) -(x, 1) in (x 10)"
    -- testInterpNl "letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 10)"

-- PROBLEM: de Bruijn indices can not deal with letrec feature