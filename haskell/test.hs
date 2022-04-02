{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except 
import Control.Monad.Except 
import Control.Monad.Identity  
-- import Control.Monad.Maybe 

data Expr 
    = Num Int 
    | Add Expr Expr 

-- type InterpM_ = MaybeT Identity 
type InterpM_ = ExceptT String Identity 
newtype InterpM a = InterpM {unInterpM :: InterpM_ a}
    deriving (Functor, Applicative, Monad, Except String)

-- runInterp m = (runMaybeT . unInterpM) m 
runInterp m = (runExceptT . unInterpM) m 

valueOf :: Expr -> InterpM Int 
valueOf (Num n) = return n 
valueOf (Add expr1 expr2) = do 
    v1 <- valueOf expr1 
    v2 <- valueOf expr2 
    if v1 == 1
        then throwError "xxx"
        else return (v1 + v2)
    return (v1 + v2)

test = do 
    runInterp (valueOf (Add (Num 1) (Num 2)))