import System.Random

data Coin    = T | H                       deriving (Eq, Show, Enum, Bounded)
data Dice    = D1 | D2 | D3 | D4 | D5 | D6 deriving (Eq, Show, Enum, Bounded)
data Outcome = Win | Lose                  deriving (Eq, Show, Enum)

class Monad m => MonadGamble m where
  toss :: m Coin
  roll :: m Dice

game :: MonadGamble m => m Outcome
game = 
  do ts <- sequenceA (replicate 6 toss)
     r  <- roll
     return (if eyes r >= heads ts then Win else Lose)
    where
      eyes :: Dice -> Int
      eyes r = case r of
        D1 -> 1
        D2 -> 2
        D3 -> 3
        D4 -> 4
        D5 -> 5
        D6 -> 6
    
      heads :: [Coin] -> Int
      heads hs = length (filter (== H) hs)
        

instance Random Coin where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random g =
    randomR (minBound, maxBound) g

instance Random Dice where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random g =
    randomR (minBound, maxBound) g

instance MonadGamble IO where
  toss :: IO Coin
  toss = randomIO :: IO Coin

  roll :: IO Dice
  roll = randomIO :: IO Dice

simulate :: IO Outcome -> Integer -> IO Rational
simulate g n =
  do games <- sequenceA (replicate (fromInteger n) g)
     return ((toRational (length (filter (== Win) games))) / (toRational (length games)))
