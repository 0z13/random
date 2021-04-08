import Data.List
import Control.Monad
import Data.Maybe 

pair :: [Int] -> Maybe Int
pair xs = listToMaybe $ do
  x:ls <- tails xs
  y    <- ls
  guard (x + y == 2020)
  pure (x*y)

triplet :: [Int] -> Maybe Int
triplet xs = listToMaybe $ do
  x:ls <- tails xs
  y:zs <- tails ls
  z    <- zs
  guard (x + y + z == 2020) 
  pure (x*y*z)

answers :: IO ()
answers = do
  xs <- map (\x -> read x :: Int) . lines <$> readFile "../resources/dat20/day01.txt"
  print $ pair xs
  putStrLn ""
  print $ triplet xs
