{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Notice: this code has nothing to do with EOPL
--  a tiny test: convert AST to IR and execute it in VM

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State 

import Text.Parsec (ParsecT, parse, spaces, string, alphaNum, many, letter, many1, digit, chainl1, try, (<|>))
import Data.Functor.Identity 
import Control.Monad (guard)
import Control.Monad.Trans.Maybe 


-- VM
type VMState   = (Int, Int)
type Output  = String
type Program = [Instr]

type VM a = ReaderT Program (WriterT Output (State VMState)) a

newtype Comp a = Comp { unComp :: VM a }
  deriving (Functor, Applicative, Monad, MonadReader Program, MonadWriter Output, MonadState VMState)

data Instr 
    = PrintA 
    | PrintB 
    | Mov Int
    | Label String
    | Jump String
    | JumpIf String 
    | Dec 
    | PrintInst String 
    | Debug
    deriving Show


label2addr :: Program -> String -> Int 
label2addr prog label= do 
    label2addr_ prog label 0
    where 
        label2addr_ :: [Instr] -> String -> Int -> Int 
        label2addr_ prog label currentAddr = case prog of
            [] -> error "label2addr failed"
            (i:is) -> case i of 
                Label name | name == label -> currentAddr
                _ -> label2addr_ is label (currentAddr + 1)


logState :: Comp ()
logState = do 
    state <- get 
    tell $ show state 


evalInstr :: Instr -> Comp ()
evalInstr instr = case instr of
    PrintA -> do
        tell "a\n"
    PrintB -> do
        tell "b\n"
    Mov n -> do 
        (pc, loopReg) <- get 
        put (pc, n)
    Dec -> do 
        (pc, loopReg) <- get 
        put (pc, loopReg - 1)
    Jump label -> do 
        prog <- ask 
        (pc, loopReg) <- get
        let jumpAddr = label2addr prog label
        put (jumpAddr, loopReg)
    JumpIf label -> do 
        prog <- ask 
        (pc, loopReg) <- get 
        let jumpAddr = label2addr prog label 
        if loopReg == 0
            then put (jumpAddr, loopReg)
            else return ()  
    PrintInst str -> do 
        tell str
        return ()
    Debug -> do 
        (pc, loopReg) <- get 
        prog <- ask 
        tell $ " End State: " ++ show (pc, loopReg)
    Label _ -> return ()



eval :: Comp ()
eval = do
    prog <- ask
    (pc, loopReg) <- get
    if pc == length prog
        then return ()
        else do
            let inst = prog !! pc
            evalInstr inst
            (pc2, loopReg2) <- get
            put (pc2 + 1, loopReg2)
            eval


execVM :: Program -> Output
execVM = flip evalState (0, 0) . execWriterT . runReaderT (unComp eval)

program :: Program
program = [
    Mov 10,
    Label "1",
    JumpIf "2",
    Dec,
    PrintA,
    Jump "1",
    Label "2",
    PrintB,
    Debug
  ]


-- parser 
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
    guard (not (elem id ["while", "print"] ))
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


data Statement 
    = Assign String Int 
    | Composite Statement Statement 
    | While Statement
    | Print String
    | EmptyStm 
    deriving Show 

parseStm :: Parser Statement 
parseStm = (chainl1 factorStm comp) <|> emptyStm

comp :: Parser (Statement -> Statement -> Statement)
comp = do 
    return $ Composite

factorStm :: Parser Statement 
factorStm 
    = try assignStm
    <|> try printStm
    <|> try whileStm

emptyStm :: Parser Statement
emptyStm = do 
    return EmptyStm

printStm :: Parser Statement
printStm = do 
    keyword "print"
    -- keyword "\""
    str <- ident
    -- keyword "\""
    keyword ";"
    return $ Print str 

assignStm :: Parser Statement 
assignStm = do 
    name <- ident
    keyword "="
    num <- positiveNum 
    keyword ";"
    return $ Assign name num 

whileStm :: Parser Statement 
whileStm = do 
    keyword "while"
    keyword "{"
    stm <- parseStm 
    keyword "}"
    return $ While stm


genLabel :: State Int String
genLabel = do 
    num <- get 
    put (num + 1)
    return $ show num 

translate :: Statement -> State Int Program
translate stm = case stm of 
    EmptyStm -> return []
    Assign var num -> return [Mov num]
    While botyStm -> do
        label1 <- genLabel
        label2 <- genLabel
        bodyInst <- translate botyStm 
        return $ [Label label1, JumpIf label2, Dec] ++ bodyInst ++ [Jump label1, Label label2]
    Print str -> do 
        return [PrintInst str]
    Composite stm1 stm2 -> do 
        inst1 <- translate stm1 
        inst2 <- translate stm2 
        return $ inst1 ++ inst2 


testTrans s = do 
    case parse parseStm "test" s of 
        Right stm ->
            execVM $ evalState (translate stm) 0

testParse s = do 
    parse parseStm "test" s 

test = do 
    testTrans "a = 3;while {print a;print b;print c;}"
