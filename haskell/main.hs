{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Writer as W
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State 
import Control.Monad.Except
import Control.Monad.Identity
import Data.Functor.Classes

data Expr = Num Float 
    | Add Expr Expr 
    | Div Expr Expr 
    deriving Show 

data Value = NumValue Float deriving Show

data Id a = Id a deriving (Show)

instance Functor Id where 
    fmap f (Id a) = Id (f a)

instance Applicative Id where 
    pure v = Id v 

instance Monad Id where 
    (Id v) >>= f = Id $ case f v of 
        (Id v2) -> v2


instance MonadFail (Either String) where 
    fail s = Left s

-- type M a = Maybe a 
-- type M a = MaybeT Id a 
-- type M a = MaybeT Identity a 
-- type M a =  ExceptT String Id a 
-- type M a = Either String a 

-- type InterpM =  ReaderT String (WriterT String (MaybeT Identity))
-- type InterpM =  ReaderT String (WriterT String (MaybeT Identity))
-- type InterpM =  ReaderT String (WriterT String (MaybeT Identity))
type InterpM =  R.ReaderT String (MaybeT (W.WriterT String Identity))
-- type InterpM =  WriterT String (ReaderT String (MaybeT Identity))
-- type M a = MaybeT (ReaderT String Identity) a 
-- type M a = MaybeT (ReaderT String (WriterT String Identity)) a 

-- deriveShow1 ''Expr

-- instance Data.Functor.Classes.Show1 Id  where 
--     liftShowsPrec 

-- runParser :: Parser a -> String -> [(a, Int)]

-- runInterp :: InterpM a -> String -> Maybe 
-- runInterp m env = runMaybeT (runWriterT (runReaderT m env))
runInterp m env =  runIdentity (W.runWriterT (runMaybeT (R.runReaderT m env)))


instance Show1 Id where
    liftShowsPrec sp _ d (Id x) = showsUnaryWith sp "Id" d x


-- instance (Show w, Show1 m) => Show1 (ReaderT w m) where
--     liftShowsPrec sp sl d (ReaderT m) =
--         showsUnaryWith (liftShowsPrec sp' sl') "ReaderT" d m
--       where
--         sp' = liftShowsPrec2 sp sl showsPrec showList
--         sl' = liftShowList2 sp sl showsPrec showList

-- instance MonadFail Id where 
--     fail s = Id $ s 

tell :: String -> InterpM ()  
tell = (lift . lift . W.tell) 

-- TODO:figout why the return Type is InterM String
ask :: InterpM String
ask = R.ask

value_of :: Expr -> InterpM Value 
value_of (Num n) = do
    tell (" Num " ++ (show n))
    return (NumValue n)
value_of (Add expr1 expr2) = do 
    tell " Add "
    (NumValue v1) <- value_of expr1 
    (NumValue v2) <- value_of expr2 
    return (NumValue (v1 + v2))
value_of (Div expr1 expr2) = do
    env <- ask 
    tell env 
    tell " Div "
    (NumValue v1) <- value_of expr1 
    (NumValue v2) <- value_of expr2 
    if v2 == 0 then do 
        tell " devide by zero"
        fail  "error: devide by zero"
    else return (NumValue (v1 / v2))

test = do 
    -- value_of (Add (Num 1) (Num 2))
    runInterp (value_of (Div (Num 1) (Num 1))) "env"

-- LEARNED: deriving Functor makes you get rid of the functor instance code 
-- [DONE] TODO: 1 write a interpreter using monad: Identity maybe either 
-- [DONE] TODO: 2 write a interpreter using monad transfer: maybeT ExceptT 
    -- | No instance for (Data.Functor.Classes.Show1 Id)
    -- | Solution1: Using Control.Monad.Identity to solve this
    -- | Solution2: Copy instance Show1 Identity to my Own Identity
-- LEARNED: 
            -- 1 fix error from example in source code, use hoogle well
            -- 2: you should import Control.Monad.Trans.Except and Control.Monad.Except at the same time
            -- 3: Except monad has 2 operations: throwError catchError

-- [DONE] TODO: 3 write a interpreter using writer monad to trace the execution of interpreter 
-- LEARNED : in the context of a kind of monad, you can use the action of that monad, for example: in writer monad context, you can
-- use tell in the writer monad
-- [DONE] TODO: combine the writer and maybe monad 
-- LEARNED: the maybe monad is on the top of writer monad, if you want to use the action of writer monad, you have to lift the action to the maybe monad context
-- to achive this, you just need to use lift function 

-- [FAILED] TODO: combine the reader monad writer monad and maybe monad, figout how to lift the action in deep :ref https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-Class.html
-- LEARNED: there is no instance Show1 ReaderT ... 
-- [FIXED] Interp is a monad, you must provide a runInterp function to run the computation
-- LEARNED : monad transformer form a monad stack, the order matters, most time the maybe monad is on the top of writer monad, and the reader monad often sit on the top of the monad stack 

-- TODO: using monad transfer from https://hackage.haskell.org/package/transformers-0.5.5.0/docs/Control-Monad-Trans-Class.html
-- TODO: using parsec to parse STLC
-- TODO: learn http://dev.stephendiehl.com/hask/#interpreters

