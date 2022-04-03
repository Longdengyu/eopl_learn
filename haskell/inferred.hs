module MyInferred where 

import Text.Parsec
import Data.Functor.Identity 
import Control.Monad (guard)
-- import Prelude hiding (lookup)
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
    | Proc Id OpType Expr 
    | Call Expr Expr
    | LetRec OpType Id Id OpType Expr Expr  
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
    ty <- parseOpType
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
    resultType <- parseOpType
    pName <- ident 
    keyword "("
    bVar <- ident 
    keyword ":"
    bVarType <- parseOpType
    keyword ")"
    keyword "="
    pBody <- parseExpr
    keyword "in"
    letrecBody <- parseExpr 
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
    -- <|> try newrefExpr
    -- <|> try derefExpr
    -- <|> try setrefExpr
    -- <|> try beginExpr

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

type Store = [Value]
type InterpM = StateT Store (ExceptT String Identity)

runInterp m = (runIdentity . runExceptT . runStateT m) []

mylookup :: Id -> Env -> InterpM Value 
mylookup _ EmptyEnv = throwError "mylookup failed" 
mylookup var (ExtendEnv name value oldEnv) = do 
    if var == name 
        then return value 
        else mylookup var oldEnv
mylookup var (ExtendEnvRec pName bVar pBody oldEnv) = do 
    if var == pName
        then return (ProcValue $ Procedure bVar pBody (ExtendEnvRec pName bVar pBody oldEnv))
        else mylookup var oldEnv 

valueOf :: Expr -> Env -> InterpM Value 
valueOf (Const n) _ = return (NumValue n)
valueOf (Var name) env = mylookup name env
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
    | VarType Int 
    deriving (Eq, Show) 

data OpType
    = NoType 
    | AType Type 
    deriving Show

parseType :: Parser Type 
parseType 
    = try intTypeExpr 
    <|> try boolTypeExpr
    <|> try procTypeExpr

parseOpType :: Parser OpType
parseOpType 
    = try noTypeExpr
    <|> try aTypeExpr

noTypeExpr :: Parser OpType
noTypeExpr = do 
    keyword "?"
    return NoType
 
aTypeExpr :: Parser OpType
aTypeExpr = do 
    ty <- parseType
    return (AType ty)

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

tmylookup :: Id -> TEnv -> InterpM Type
tmylookup _ EmptyTEnv = throwError "mylookup in tenv failed"
tmylookup var (ExtendTEnv name value oldTenv) = 
    if var == name 
        then return value 
        else tmylookup var oldTenv


-- type inference

applyOneSubst :: Type -> Type -> Type -> Type
applyOneSubst ty0 (VarType var) ty1 = do 
    case ty0 of 
        IntType -> IntType
        BoolType -> BoolType
        (ProcType argType resultType) -> (ProcType 
                                            (applyOneSubst argType (VarType var) ty1)
                                            (applyOneSubst resultType (VarType var) ty1))
        (VarType sn) -> do 
            if ty0 == (VarType sn)
                then ty1 
                else ty0 
applyOneSubst _ _ _ = error "the second argument must be VarType"

type Subst = [(Int, Type)]
emptySubst :: Subst 
emptySubst = []
extendSubst :: Subst -> Type -> Type -> Subst 
extendSubst subst (VarType sn) ty = do 
    let subst2 = map (\(itemSn, itemTy) -> (itemSn, (applyOneSubst 
                                                        itemTy 
                                                        (VarType sn) 
                                                        ty))) 
                    subst
    (sn, ty):subst2
extendSubst _ _ _ = error "extendSubst second argument must be VarType"

applySubstToType :: Type -> Subst -> Type 
applySubstToType IntType _ = IntType
applySubstToType BoolType _ = BoolType 
applySubstToType (ProcType argType resultType) subst = ProcType (applySubstToType argType subst)
                                                                (applySubstToType resultType subst)
applySubstToType (VarType sn) subst = case (lookup sn subst) of 
    Just ty -> ty
    Nothing -> error "applySubstToType: lookup failed"

noOccurrence :: Type -> Type -> Bool 
noOccurrence (VarType sn) ty = case ty of 
    IntType -> True 
    BoolType -> True 
    (ProcType argType resultType) -> do 
        (&&) (noOccurrence (VarType sn) argType)
             (noOccurrence (VarType sn) resultType)
    (VarType sn2) -> not (sn == sn2)
noOccurrence _ _ = error "the firest argument must be varType"

unifier :: Type -> Type -> Subst -> Expr -> Subst
unifier ty1 ty2 subst expr = do 
    let ty1 = applySubstToType ty1 subst
    let ty2 = applySubstToType ty2 subst
    case (ty1, ty2) of 
        (ty1, ty2) | ty1 == ty2 -> subst
        (VarType sn, ty2) -> do 
            if noOccurrence ty1 ty2 
                then extendSubst subst ty1 ty2 
                else error "no-occurrence-violation"
        (ty1, VarType sn) -> do 
            if noOccurrence ty2 ty1 
                then extendSubst subst ty2 ty1 
                else error "no-occurrence-violation"
        ((ProcType argType1 resultType1), (ProcType argType2 resultType2)) -> do 
            let subst = unifier argType1 argType2 subst expr
            let subst = unifier resultType1 resultType2 subst expr 
            subst
        otherwise -> error "unification-failure"



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

test = do 
    -- testInterp "let a = newref (123) in begin setref (a 999); setref (a 100); deref (a) end"
    -- testTypeOf "letrec int double (x : int) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 2)"
    -- parse parseType "test" "( int -> (int -> bool))"
    -- testInterp "zero?(zero?(1))"
    testParse "proc (x:?) x"
 

-- [DONE] TODO: using optional type annatation 
-- TODO: doing unification 
-- TODO: EOPL page 288:Finding the Type of an Expression
