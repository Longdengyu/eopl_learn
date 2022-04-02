module MySimpleType where 

import Text.Parsec
import Data.Functor.Identity 
import Control.Monad (guard)
import Prelude hiding (lookup)
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.State 
import Control.Monad.Trans.Except  
import Control.Monad.Except  


-- AST 

type Id = String 
data Expr 
    = Const Int 
    | Let Id Expr Expr 
    | Var Id 
    | If Expr Expr Expr 
    | Diff Expr Expr 
    | Zero Expr 
    | Proc Id Type Expr 
    | Call Expr Expr
    | LetRec Type Id Id Type Expr Expr  
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
    keyword ":"
    ty <- parseType
    keyword ")"
    body <- parseExpr 
    return (Proc id ty body)

callExpr = do 
    keyword "("
    rator <- parseExpr 
    rand <- parseExpr 
    keyword ")"
    return (Call rator rand)

letrecExpr = do 
    keyword "letrec"
    resultType <- parseType
    pName <- ident 
    keyword "("
    bVar <- ident 
    keyword ":"
    bVarType <- parseType
    keyword ")"
    -- bVar <- keyword "(" *> ident <* keyword ")"
    keyword "="
    pBody <- parseExpr
    keyword "in"
    letrecBody <- parseExpr 
    -- return (LetRec pName bVar pBody letrecBody)
    return (LetRec resultType pName bVar bVarType pBody letrecBody)

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
-- type InterpM = StateT Store (MaybeT Identity)
type InterpM = StateT Store (ExceptT String Identity)

-- instance MonadFail Identity where 
--     throwError s = Identity s

-- runInterp = runIdentity . runMaybeT
-- runInterp m = (runIdentity . runMaybeT . runStateT m) []
runInterp m = (runIdentity . runExceptT . runStateT m) []

lookup :: Id -> Env -> InterpM Value 
lookup _ EmptyEnv = throwError "lookup failed" 
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
    value <- valueOf expr1 env 
    case value of 
        NumValue n -> return (BoolValue (n == 0))
        _ -> throwError "Zero not num"

valueOf (Let var bindExpr bodyExpr) env = do 
    bindValue <- valueOf bindExpr env 
    valueOf bodyExpr (ExtendEnv var bindValue env)
valueOf (If expr1 expr2 expr3) env = do 
    v1 <- valueOf expr1 env 
    case v1 of 
        BoolValue b -> do 
            if b 
                then valueOf expr2 env 
                else valueOf expr3 env 
        _ -> throwError "If pred is not Bool Value"

valueOf (Diff expr1 expr2) env = do 
    v1 <- valueOf expr1 env 
    v2 <- valueOf expr2 env 
    case (v1, v2) of 
        (NumValue num1, NumValue num2) -> return (NumValue (num1 - num2))
        _ -> throwError "Diff not Number"
valueOf (Proc var ty body) env = return (ProcValue (Procedure var body env))
valueOf (Call rator rand) env = do 
    procValue <- valueOf rator env 
    bindValue <- valueOf rand env 
    case procValue of 
        ProcValue (Procedure bindVar body pEnv) -> valueOf body (ExtendEnv bindVar bindValue pEnv)
        _ -> throwError "Call rator not procedure"

valueOf (LetRec resultType pName bVar bVarType pBody letrecBody) env = valueOf letrecBody  (ExtendEnvRec pName bVar pBody env)
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
    v1 <- valueOf expr1 env 
    case v1 of 
        RefValue index -> do 
            store <- get 
            return (store !! index)
        _ -> throwError "Deref not RefValue"
valueOf (SetRef expr1 expr2) env = do 
    v1 <- valueOf expr1 env 
    case v1 of 
        RefValue index -> do 
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
        _ -> throwError "SetRef: not RefValue"

-- type 
data Type
    = IntType
    | BoolType 
    | ProcType Type Type 
    deriving (Eq, Show) 

parseType :: Parser Type 
parseType 
    = try intTypeExpr 
    <|> try boolTypeExpr
    <|> try procTypeExpr

intTypeExpr :: Parser Type
intTypeExpr = do 
    keyword "int"
    return IntType

boolTypeExpr :: Parser Type 
boolTypeExpr = do 
    keyword "bool"
    return BoolType 

procTypeExpr :: Parser Type 
procTypeExpr = do 
    keyword "("
    argType <- parseType
    keyword "->"
    resultType <- parseType
    keyword ")"
    return (ProcType argType resultType)

data TEnv 
    = EmptyTEnv 
    | ExtendTEnv Id Type TEnv 
    deriving Show 

tlookup :: Id -> TEnv -> InterpM Type
tlookup _ EmptyTEnv = throwError "lookup in tenv failed"
tlookup var (ExtendTEnv name value oldTenv) = 
    if var == name 
        then return value 
        else tlookup var oldTenv

typeOf :: Expr -> TEnv -> InterpM Type 
typeOf (Const n) _ = return IntType
typeOf (Var name) tenv = tlookup name tenv
typeOf (Zero expr1) tenv = do 
    ty1 <- typeOf expr1 tenv 
    if ty1 == IntType
        then return BoolType
        else throwError "type in Zero Expr not equal"
typeOf (Let var bindExpr bodyExpr) tenv = do 
    bindValue <- typeOf bindExpr tenv 
    typeOf bodyExpr (ExtendTEnv var bindValue tenv)
typeOf (If expr1 expr2 expr3) tenv = do 
    tyPred <- typeOf expr1 tenv
    case tyPred of 
        BoolType -> do  
            ty2 <- typeOf expr2 tenv 
            ty3 <- typeOf expr3 tenv
            if ty2 == ty3 
                then return ty2 
                else throwError "type in If not equal"
        _ -> throwError "If pred must be BoolType"

typeOf (Diff expr1 expr2) tenv = do 
    ty1 <- typeOf expr1 tenv 
    ty2 <- typeOf expr2 tenv 
    if ty1 == IntType && ty2 == IntType
        then return IntType
        else throwError "type in Diff not IntType"
typeOf (Proc var tyVar body) tenv = do 
    tyRes <- typeOf body (ExtendTEnv var tyVar tenv)
    return (ProcType tyVar tyRes)
typeOf (Call rator rand) tenv = do 
    tyRator <- typeOf rator tenv 
    case tyRator of 
        ProcType tyVar tyRes -> do 
            bindType <- typeOf rand tenv
            if bindType /= tyVar
                then throwError "type in Call not equal"
                else return  tyRes
        _ -> throwError "rator is not ProcType"
typeOf (LetRec resultType pName bVar bVarType pBody letrecBody) tenv = do 
    tyProcBody <- typeOf pBody (ExtendTEnv bVar bVarType (ExtendTEnv pName (ProcType bVarType resultType) tenv))
    if tyProcBody /= resultType
        then throwError "type in LetRec fail"
        else do 
            tyLetRecBody <- typeOf letrecBody (ExtendTEnv pName (ProcType bVarType resultType) tenv)
            return tyLetRecBody
typeOf (Begin exprs) tenv = case exprs of 
    x:[] -> typeOf x tenv
    x:xs -> do 
        typeOf x tenv 
        typeOf (Begin xs) tenv 
-- typeOf (NewRef expr1) tenv = do 
--     value <- typeOf expr1 tenv 
--     store <- get 
--     let nextRef = length store 
--     put (store ++ [value])
--     return (RefValue nextRef)
-- typeOf (DeRef expr1) tenv = do 
--     RefValue index <- typeOf expr1 tenv 
--     store <- get 
--     return (store !! index)
-- typeOf (SetRef expr1 expr2) tenv = do 
--     RefValue index <- typeOf expr1 tenv 
--     value <- typeOf expr2 tenv 
--     store <- get 
--     put (replaceNth index value store)
--     return UnitValue
--     where 
--         replaceNth :: Int -> a -> [a] -> [a]
--         replaceNth _ _ [] = []
--         replaceNth n newVal (x:xs)
--             | n == 0 = newVal:xs
--             | otherwise = x:replaceNth (n-1) newVal xs


-- test 

testInterp s = do 
    case (parse parseExpr "testInterp" s) of 
        Right expr ->  do 
            putStr $ show $ runInterp (valueOf expr EmptyEnv)
            putStr "\n"
        Left err -> do 
            putStr $ show err
            putStr "\n"

testParse s = do 
    parse parseExpr "test" s 

testTypeOf s = do 
    case (parse parseExpr "testInterp" s) of 
        Right expr -> do 
            putStr $ show $ runInterp (typeOf expr EmptyTEnv)
            putStr "\n"
        Left err -> do 
            putStr $ show err
            putStr "\n"

test = do 
    -- testInterp "let a = newref (123) in begin setref (a 999); setref (a 100); deref (a) end"
    testTypeOf "letrec int double (x : int) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 2)"
    -- parse parseType "test" "( int -> (int -> bool))"
    -- testInterp "zero?(zero?(1))"
 

-- TODO: know how to fix this problem 
-- TODO: using test framework to test the code 
-- ref: https://stackoverflow.com/questions/33057481/why-does-it-seem-that-the-parsec-choice-operator-depends-on-order-of-the-parsers
-- parsec sacrifices perfect nondeterminism
-- TODO: using Except monad