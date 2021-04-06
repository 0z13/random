import Data.List
import Control.Monad
import Data.Maybe 

pair :: [Int] -> Maybe Int
pair xs = listToMaybe $ do
  x:ls <- tails xs
  y    <- ls
  guard (x + y == 2020)
  pure (x*y)
