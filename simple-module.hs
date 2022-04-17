-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module MyModuleLang where 

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
    | QualifiedVar Id Id  
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
    guard (not (elem id ["let", "in", "if", "then", "else", "proc", 
                        "zero", "letrec", "newref","deref", "setref", 
                        "begin", "end", "from", "take", "module", "interface", "body"] ))
    return id 

positiveNum :: Parser Int 
positiveNum = do
    num <- thetoken (many1 digit)
    return (read num::Int)

negativeNum :: Parser Int 
negativeNum = do 
    keyword "-"
    num <- positiveNum
    return $ -num

constExpr :: Parser Expr 
constExpr = do 
    num <- (positiveNum <|> negativeNum)
    return $ Const num

varExpr = Var <$> ident 
letExpr = do 
    keyword "let"
    name <- ident 
    keyword "="
    expr1 <- parseExpr 
    keyword "in"
    expr2 <- parseExpr 
    return $ Let name expr1 expr2

ifExpr = do 
    keyword "if" 
    expr1 <- parseExpr
    keyword "then"
    expr2 <- parseExpr 
    keyword "else"
    expr3 <- parseExpr 
    return $ If expr1 expr2 expr3

diffExpr = do 
    keyword "-"
    keyword "("
    expr1 <- parseExpr 
    keyword ","
    expr2 <- parseExpr 
    keyword ")"
    return $ Diff expr1 expr2

zeroExpr = do 
    keyword "zero?"
    keyword "("
    expr1 <- parseExpr 
    keyword ")"
    return $ Zero expr1

procExpr = do 
    keyword "proc"
    keyword "("
    id <- ident 
    keyword ":"
    ty <- parseOpType
    keyword ")"
    body <- parseExpr 
    return $ Proc id ty body

callExpr = do 
    keyword "("
    rator <- parseExpr 
    rand <- parseExpr 
    keyword ")"
    return $ Call rator rand

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
    return $ LetRec resultType pName bVar bVarType pBody letrecBody

newrefExpr = do 
    keyword "newref"
    expr1 <- (keyword "(" *> parseExpr <* keyword ")")
    return $ NewRef expr1

derefExpr = do 
    keyword "deref"
    expr1 <- keyword "(" *> parseExpr <* keyword ")"
    return $ DeRef expr1

setrefExpr = do 
    keyword "setref"
    keyword "("
    expr1 <- parseExpr 
    expr2 <- parseExpr
    keyword ")"
    return $ SetRef expr1 expr2

beginExpr = do 
    keyword "begin"
    exprs <- sepBy1 parseExpr (keyword ";")
    keyword "end"
    return $ Begin exprs

qualifiedVarExpr = do 
    keyword "from" 
    mName <- ident
    keyword "take"
    varName <- ident 
    return $ QualifiedVar mName varName

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
    <|> try qualifiedVarExpr
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
    | MValue ModuleValue -- TODO: the value is expvalue modulevalue sit here is not appropriate 
    deriving Show 

data Env 
    = EmptyEnv 
    | ExtendEnv Id Value Env 
    | ExtendEnvRec Id Id Expr Env 
    deriving Show 

type Store = [Value]
-- type InterpM = StateT Store (ExceptT String Identity)
type InterpM = StateT Store (ExceptT String (W.WriterT String Identity))

-- type TypeCheckerM = StateT Int (ExceptT String Identity)
type TypeCheckerM = StateT Int (ExceptT String (W.WriterT String Identity))

runInterp m = (runIdentity . W.runWriterT . runExceptT . runStateT m) []
-- runInterp m = (runIdentity . runExceptT . runStateT m) []
runTypeChecker m = (runIdentity . W.runWriterT . runExceptT . runStateT m) 0

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
valueOf expr env = case expr of 
    Const n -> return (NumValue n)
    Var name -> mylookup name env
    Zero expr1 -> do 
        value <- valueOf expr1 env 
        case value of 
            NumValue n -> return (BoolValue (n == 0))
            _ -> throwError "Zero not num"
    Let var bindExpr bodyExpr -> do 
        bindValue <- valueOf bindExpr env 
        valueOf bodyExpr (ExtendEnv var bindValue env)
    If expr1 expr2 expr3 -> do 
        v1 <- valueOf expr1 env 
        case v1 of 
            BoolValue b -> do 
                if b 
                    then valueOf expr2 env 
                    else valueOf expr3 env 
            _ -> throwError "If pred is not Bool Value"
    Diff expr1 expr2 -> do 
        v1 <- valueOf expr1 env 
        v2 <- valueOf expr2 env 
        case (v1, v2) of 
            (NumValue num1, NumValue num2) -> return (NumValue (num1 - num2))
            _ -> throwError "Diff not Number"
    Proc var ty body -> return $ ProcValue (Procedure var body env)
    Call rator rand -> do 
        procValue <- valueOf rator env 
        bindValue <- valueOf rand env 
        case procValue of 
            ProcValue (Procedure bindVar body pEnv) -> valueOf body (ExtendEnv bindVar bindValue pEnv)
            _ -> throwError "Call rator not procedure"

    LetRec resultType pName bVar bVarType pBody letrecBody -> valueOf letrecBody  (ExtendEnvRec pName bVar pBody env)
    Begin exprs -> case exprs of 
        x:[] -> valueOf x env
        x:xs -> do 
            valueOf x env 
            valueOf (Begin xs) env
        [] -> error "forbidden" 
    NewRef expr1 -> do 
        value <- valueOf expr1 env 
        store <- get 
        let nextRef = length store 
        put (store ++ [value])
        return (RefValue nextRef)
    DeRef expr1 -> do 
        v1 <- valueOf expr1 env 
        case v1 of 
            RefValue index -> do 
                store <- get 
                return (store !! index)
            _ -> throwError "Deref not RefValue"
    SetRef expr1 expr2 -> do 
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
    QualifiedVar mName varName -> do 
        value <- mylookup mName env
        case value of 
            MValue mValue -> applyModule mValue varName
            _ -> error "forbidden"
-- type 
data Type
    = IntType
    | BoolType 
    | ProcType Type Type 
    | VarType Int 
    | ModuleType TEnv 
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
    | ExtendTEnvWithModule Id Type TEnv 
    deriving (Eq, Show)

tmylookup :: Id -> TEnv -> TypeCheckerM Type
tmylookup _ EmptyTEnv = throwError "mylookup in tenv failed"
tmylookup var (ExtendTEnv name value oldTenv) = 
    if var == name 
        then return value 
        else tmylookup var oldTenv
tmylookup var (ExtendTEnvWithModule mname mType oldEnv) = 
    if var == mname 
        then case mType of 
            (ModuleType _) -> return mType
            _ -> throwError "value must be ModuleType"
        else tmylookup var oldEnv 

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


tell :: String -> TypeCheckerM ()  
tell = (lift . lift . W.tell)

mytell :: String -> InterpM ()  
mytell = (lift . lift . W.tell)

applySubstToType :: Type -> Subst -> TypeCheckerM Type 
applySubstToType ty subst = case ty of 
    IntType -> return IntType
    BoolType -> return BoolType
    ProcType argType resultType -> do 
        argType_ <- applySubstToType argType subst
        resultType_ <- applySubstToType resultType subst
        return $ ProcType argType_ resultType_
    VarType sn -> do 
        case lookup sn subst of 
            Just lookupResult -> return lookupResult
            Nothing -> return ty


noOccurrence :: Type -> Type -> Bool 
noOccurrence (VarType sn) ty = case ty of 
    IntType -> True 
    BoolType -> True 
    (ProcType argType resultType) -> do 
        (&&) (noOccurrence (VarType sn) argType)
             (noOccurrence (VarType sn) resultType)
    (VarType sn2) -> not (sn == sn2)
noOccurrence _ _ = error "the firest argument must be varType"

unifier :: Type -> Type -> Subst -> Expr -> TypeCheckerM Subst
unifier ty1 ty2 subst expr = do 
    ty1_ <- applySubstToType ty1 subst
    ty2_ <- applySubstToType ty2 subst
    case (ty1_, ty2_) of 
        (ty1_, ty2_) | ty1_ == ty2_ -> return subst
        (VarType sn, ty2_) -> do 
            if noOccurrence ty1_ ty2_ 
                then return $ extendSubst subst ty1_ ty2_ 
                else throwError "no-occurrence-violation"
        (ty1_, VarType sn) -> do 
            if noOccurrence ty2_ ty1_ 
                then return $ extendSubst subst ty2_ ty1_ 
                else throwError "no-occurrence-violation"
        ((ProcType argType1 resultType1), (ProcType argType2 resultType2)) -> do 
            subst1 <- unifier argType1 argType2 subst expr
            subst2 <- unifier resultType1 resultType2 subst1 expr 
            return subst2
        otherwise -> throwError "unification-failure"



freshTVarType :: TypeCheckerM Type
freshTVarType = do 
    num <- get 
    put (num + 1)
    return (VarType num)

opTypeToType :: OpType -> TypeCheckerM Type
opTypeToType NoType = freshTVarType
opTypeToType (AType ty) = return ty 

newtype Answer = Answer { unAnswer ::(Type, Subst)} deriving Show 

typeOf :: Expr -> TEnv -> Subst -> TypeCheckerM Answer
typeOf expr tEnv subst = case expr of 
    Const _ -> return (Answer (IntType, subst)) 
    Zero expr1 -> do 
        Answer (ty1, subst1) <- typeOf expr1 tEnv subst
        subst2 <- unifier ty1 IntType subst1 (Zero expr1)
        return $ Answer (BoolType, subst2)     
    Diff expr1 expr2 -> do 
        Answer (ty1, subst1) <- typeOf expr1 tEnv subst
        subst1_ <- unifier ty1 IntType subst1 expr1
        Answer (ty2, subst2) <- typeOf expr2 tEnv subst1_
        subst2_ <- unifier ty2 IntType subst2 expr2 
        return $ Answer (IntType, subst2_)   
    If expr1 expr2 expr3 -> do 
        Answer (ty1, subst1) <- typeOf expr1 tEnv subst
        subst2 <- unifier ty1 BoolType subst1 expr1 
        Answer (ty2, subst3) <- typeOf expr2 tEnv subst2
        Answer (ty3, subst4) <- typeOf expr3 tEnv subst3
        subst5 <- unifier ty2 ty3 subst4 (If expr1 expr2 expr3)
        return $ Answer (ty2, subst5)
    Var name -> do 
        ty <- tmylookup name tEnv
        return $ Answer (ty, subst) 
    Let var bindExpr bodyExpr -> do 
        Answer (tyBind, subst1) <- typeOf bindExpr tEnv subst
        typeOf bodyExpr (ExtendTEnv var tyBind tEnv) subst1
    Proc arg opType bodyExpr -> do 
        argType <- opTypeToType opType  
        Answer (bodyType, subst1) <- typeOf bodyExpr (ExtendTEnv arg argType tEnv) subst 
        return $ Answer ((ProcType argType bodyType), subst1)
    Call rator rand -> do
        resultType <- freshTVarType
        Answer (ratorType, subst1) <- typeOf rator tEnv subst
        Answer (randType, subst2) <- typeOf rand tEnv subst1
        subst3 <- unifier ratorType (ProcType randType resultType) subst2 expr
        return $ Answer (resultType, subst3)
    LetRec pResultOpType pName bVar bVarOpType pBodyExpr letRecBodyExpr -> do 
        pResultType <- opTypeToType pResultOpType
        bVarType <- opTypeToType bVarOpType
        let tEnvForLetRecBody = ExtendTEnv pName (ProcType bVarType pResultType) tEnv
        Answer (pBodyType, subst1) <- typeOf pBodyExpr (ExtendTEnv bVar bVarType tEnvForLetRecBody) subst 
        subst2 <- unifier pBodyType pResultType subst1 pBodyExpr
        typeOf letRecBodyExpr tEnvForLetRecBody subst2
    QualifiedVar mName varName -> do 
        mType <- tmylookup mName tEnv
        case mType of 
            ModuleType innerEnv -> do
                ty <- tmylookup varName innerEnv
                return $ Answer (ty, subst)



-- module

data Program = Program [ModuleDefn] Expr deriving Show  
data ModuleDefn = ModuleDefn Id Iface ModuleBody deriving Show
data Iface = Iface [Decl] deriving Show
data ModuleBody = ModuleBody [Defn] deriving Show
data Defn = Defn Id Expr deriving Show
data Decl = Decl Id Type deriving Show


parseDecl :: Parser Decl 
parseDecl = do 
    varName <- ident 
    keyword ":"
    ty <- parseType
    return $ Decl varName ty 

parseDefn :: Parser Defn 
parseDefn = do 
    varName <- ident
    keyword "="
    exp <- parseExpr
    return $ Defn varName exp 

parseModuleBody :: Parser ModuleBody
parseModuleBody = do 
    keyword "["
    defns <- many parseDefn
    keyword "]"
    return $ ModuleBody defns

parseIface :: Parser Iface
parseIface = do 
    keyword "["
    decls <- many parseDecl
    keyword "]"
    return $ Iface decls 

parseModuleDefn :: Parser ModuleDefn
parseModuleDefn = do 
    keyword "module"
    mName <- ident 
    keyword "interface"
    expectedIFace <- parseIface
    keyword "body"
    mBody <- parseModuleBody
    return $ ModuleDefn mName expectedIFace mBody

parseProgram :: Parser Program
parseProgram = do 
    mDefs <- many parseModuleDefn
    body <- parseExpr
    return $ Program mDefs body 

data ModuleValue = ModuleValue Id [Id] Env deriving Show 


applyModule :: ModuleValue -> Id -> InterpM Value 
applyModule (ModuleValue _ exportNames env) name = do 
    if elem name exportNames
        then mylookup name env
        else throwError "lookup failed in ModuleValue" 


valueOfModule :: ModuleDefn -> Env -> InterpM Value 
valueOfModule (ModuleDefn mName expectedIFace (ModuleBody defns)) env = do 
    let decls = ifaceToDecls expectedIFace
    let names = declsToNames decls []
    mEnv <- valueOfDefns defns env
    return $ MValue $ ModuleValue mName names mEnv
    where 
        ifaceToDecls :: Iface -> [Decl] 
        ifaceToDecls (Iface decls) = decls 

        declsToNames :: [Decl] -> [String] -> [String]
        declsToNames [] names = names 
        declsToNames ((Decl name _):xs) names = name:(declsToNames xs names)

valueOfDefns :: [Defn] -> Env -> InterpM Env 
valueOfDefns [] env = return $ env 
valueOfDefns ((Defn name bindExpr):xs) env = do 
    value <- valueOf bindExpr env 
    valueOfDefns xs (ExtendEnv name value env)

-- let* sematics
valueOfProgram :: Program -> InterpM Value
valueOfProgram (Program mDefs body) = do
    env <- valueOfModuleDefns mDefs EmptyEnv
    -- mytell $ show env
    valueOf body env 

valueOfModuleDefns :: [ModuleDefn] -> Env -> InterpM Env 
valueOfModuleDefns [] env = return $ env 
valueOfModuleDefns (m@(ModuleDefn mName expectedIFace mBody):xs)  env = do 
    mValue <- valueOfModule m env 
    valueOfModuleDefns xs (ExtendEnv mName mValue env)


-- typeOf Module 
typeOfProgram :: Program -> TypeCheckerM Answer 
typeOfProgram (Program mDefs body) = do
    Answer (ty, subst1) <- typeOfModuleDefns mDefs EmptyTEnv emptySubst
    case ty of 
        ModuleType tEnv -> typeOf body tEnv subst1
        _ -> error "forbiden"

typeOfModuleDefns :: [ModuleDefn] -> TEnv -> Subst -> TypeCheckerM Answer
typeOfModuleDefns mDefns tEnv subst = case mDefns of 
    [] -> return $ Answer ((ModuleType tEnv), subst)
    (moduleDefn@(ModuleDefn mName _ _):rest) -> do 
        Answer (tyModule, subst1) <- typeOfModule moduleDefn tEnv subst
        typeOfModuleDefns rest (ExtendTEnv mName tyModule tEnv) subst1

-- data ModuleDefn = ModuleDefn Id Iface ModuleBody deriving Show
typeOfModule :: ModuleDefn -> TEnv -> Subst -> TypeCheckerM Answer
typeOfModule (ModuleDefn mName mIface mBody) tEnv subst = do 
    let ifaceEnv = mIface2Env mIface
    (bodyEnv, subst1) <- mBody2Env mBody tEnv subst
    satisfy <- bodySatsifyIface bodyEnv ifaceEnv -- TODO: catch error
    if satisfy
        then return $ Answer (ModuleType ifaceEnv, subst1)
        else throwError "bodySatsifyIface check failed"

mIface2Env :: Iface -> TEnv
mIface2Env (Iface decls) = case decls of 
    [] -> EmptyTEnv
    ((Decl name ty):rest) -> ExtendTEnv name ty (mIface2Env (Iface rest))

mBody2Env :: ModuleBody -> TEnv -> Subst -> TypeCheckerM (TEnv, Subst)
mBody2Env (ModuleBody defns) tEnv subst = case defns of 
    [] -> return (tEnv, subst)
    ((Defn name exp):rest) -> do 
        Answer (ty, subst1) <- typeOf exp tEnv subst
        mBody2Env (ModuleBody rest) (ExtendTEnv name ty tEnv) subst1

bodySatsifyIface :: TEnv -> TEnv -> TypeCheckerM Bool 
bodySatsifyIface bodyEnv iFaceEnv = case iFaceEnv of
    EmptyTEnv -> return True 
    ExtendTEnv name ty oldEnv -> do
        tyInBody <- tmylookup name bodyEnv
        if ty /= tyInBody
            then return False 
            else bodySatsifyIface bodyEnv oldEnv

-- test 
testInterp s = do 
    case parse parseProgram "test" s of 
        Right prog@(Program mDefs body) ->  do 
            putStr $ show $ runInterp (valueOfProgram prog)
            putStr "\n"
        Left err -> do 
            putStr $ show err
            putStr "\n"


testParse s = do 
    parse parseExpr "test" s 


testTypeOf s = do 
    case (parse parseExpr "testTypeOf" s) of 
        Right expr ->  do 
            putStr $ show $ runTypeChecker (typeOf expr EmptyTEnv emptySubst)
            putStr "\n"
        Left err -> do 
            putStr $ show err
            putStr "\n"

testParseProgram s = do 
    parse parseProgram "test" s 


testTypeOfProg s = do 
    case parse parseProgram "test" s of 
       Right prog -> runTypeChecker (typeOfProgram prog) 

test = do 
    -- testInterp "module m1 interface [a:int b:bool] body [a = 111 b = zero?(0)] module m2 interface [a:int b:bool] body [a = from m1 take b b = zero?(1)] from m2 take a"
    testTypeOfProg "module m1 interface [a: int b:bool] body [a = 1 b=zero?(1)] from m1 take b"
    -- testInterp "module m1 interface [a: int b:bool] body [a = 1 b=zero?(0)] if from m1 take b then 1 else 2"