import Control.Monad (guard)
import Control.Monad (MonadPlus, mplus)

data CoinType = Fair | Biased deriving (Show)

data Coin = Head | Tail deriving (Eq,Show)

toss Fair   = [Head, Tail]
toss Biased = [Head, Head]

pick = [Fair, Biased]

experiment = do
    coin   <- pick         -- Pick a coin at random
    result <- toss coin    -- Toss it, to get a result
    guard (result == Head) -- We only care about results that come up Heads
    return coin  

nums = [1, 2, 3, 4, 5]

test = do 
    num <- nums 
    return (num + 1)

coin :: MonadPlus m => m Int
coin = return 0 `mplus` return 1

coin_ :: MonadPlus m => m Int
coin_ | x+y > 0 = x
    where 
        x = coin
        y = coin

-- coin_ :: MonadPlus m => m Int
-- coin_ = do 
--     x <- coin
--     y <- coin 
--     guard (x + y > 0)
--     return x
