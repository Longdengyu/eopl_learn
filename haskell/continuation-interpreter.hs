module MyContinuation where 

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
    | Proc Id Expr 
    | Call Expr Expr
    | LetRec Id Id Expr Expr  
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
                        "zero", "letrec"] ))
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
    keyword ")"
    body <- parseExpr 
    return $ Proc id body

callExpr = do 
    keyword "("
    rator <- parseExpr 
    rand <- parseExpr 
    keyword ")"
    return $ Call rator rand

letrecExpr = do 
    keyword "letrec"
    pName <- ident 
    keyword "("
    bVar <- ident 
    keyword ")"
    keyword "="
    pBody <- parseExpr
    keyword "in"
    letrecBody <- parseExpr 
    return $ LetRec pName bVar pBody letrecBody

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


-- Interpreter 
data Procedure = Procedure Id Expr Env deriving Show 
data Value 
    = NumValue Int 
    | BoolValue Bool 
    | ProcValue Procedure 
    | UnitValue 
    deriving Show 

data Env 
    = EmptyEnv 
    | ExtendEnv Id Value Env 
    | ExtendEnvRec Id Id Expr Env 
    deriving Show 

type Store = [Value]
type InterpM = StateT Store (ExceptT String (W.WriterT String Identity))


runInterp m = (runIdentity . W.runWriterT . runExceptT . runStateT m) []

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
    Proc var body -> return $ ProcValue (Procedure var body env)
    Call rator rand -> do 
        procValue <- valueOf rator env 
        bindValue <- valueOf rand env 
        case procValue of 
            ProcValue (Procedure bindVar body pEnv) -> valueOf body (ExtendEnv bindVar bindValue pEnv)
            _ -> throwError "Call rator not procedure"
    LetRec  pName bVar  pBody letrecBody -> valueOf letrecBody  (ExtendEnvRec pName bVar pBody env)


mytell :: String -> InterpM ()  
mytell = (lift . lift . W.tell)



-- continuation 
data Cont 
    = EndCont
    | ZeroCont Cont
    | IfCont Expr Expr Env Cont
    | LetCont Id Expr Env Cont 
    | Diff1Cont Expr Env Cont
    | Diff2Cont Value Cont 
    | ProcCont Value Cont 
    | RatorCont Expr Env Cont 
    | RandCont Value Cont

applyCont :: Cont -> Value -> InterpM Value
applyCont cont val = case cont of 
    EndCont -> do 
        mytell $ " apply EndCont with: " ++ show val 
        return $ val
    ZeroCont savedCont -> do 
        -- mytell $ " apply ZeroCont with: " ++ show val 
        case val of 
            NumValue n -> applyCont savedCont (BoolValue (n == 0))
            -- _ -> throwError " Zero not num "
    IfCont thenExpr elseExpr savedEnv savedCont -> do 
        -- mytell $ " apply IfCont with: " ++ show val
        case val of 
            BoolValue b -> do 
                if b
                    then valueOfK thenExpr savedEnv savedCont 
                    else valueOfK elseExpr savedEnv savedCont
            _ -> throwError "pred not Bool"
    LetCont var body savedEnv savedCont -> do 
        valueOfK body (ExtendEnv var val savedEnv) savedCont
    Diff1Cont expr2 savedEnv savedCont -> do
        valueOfK expr2 savedEnv diff2Cont 
        where 
            diff2Cont = Diff2Cont val savedCont
    Diff2Cont value1 savedCont -> case (value1, val) of 
        (NumValue num1, NumValue num2) ->  applyCont savedCont (NumValue (num1 - num2))
    RatorCont randExpr savedEnv savedCont -> do 
        valueOfK randExpr savedEnv randCont
        where 
            randCont = RandCont val savedCont
    RandCont procValue savedCont -> case procValue of 
        ProcValue (Procedure bindVar body pEnv) -> do 
            valueOfK body (ExtendEnv bindVar val pEnv) savedCont
        _ -> throwError "the value must be ProcValue"

valueOfK :: Expr -> Env -> Cont -> InterpM Value
valueOfK expr env cont = case expr of 
    Const n -> applyCont cont (NumValue n)
    Var name -> do 
        val <- mylookup name env
        applyCont cont val 
    Zero expr1 -> do 
        let zeroCont = ZeroCont cont
        valueOfK expr1 env zeroCont
    Let var bindExpr bodyExpr -> do 
        let letCont = LetCont var bodyExpr env cont
        valueOfK bindExpr env letCont
    If expr1 expr2 expr3 -> do
        let ifCont = IfCont expr2 expr3 env cont 
        valueOfK expr1 env ifCont  
    Diff expr1 expr2 -> do
        valueOfK expr1 env diff1Cont
        where 
            diff1Cont = Diff1Cont expr2 env cont 
    Proc var body -> do
        let procValue = (ProcValue (Procedure var body env))
        applyCont cont procValue
    Call rator rand -> do
        valueOfK rator env ratorCont
        where 
            ratorCont = RatorCont rand env cont  
    LetRec pName bVar pBody letrecBody -> do 
        valueOfK letrecBody (ExtendEnvRec pName bVar pBody env) cont 

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


testValueOfK s = do 
    case (parse parseExpr "testInterp" s) of 
        Right expr ->  do 
            putStr $ show $ runInterp (valueOfK expr EmptyEnv EndCont)
            putStr "\n"
        Left err -> do 
            putStr $ show err
            putStr "\n"

test = do 
    testValueOfK "letrec double (x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 20)"
