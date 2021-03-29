import qualified Data.Set as S
import Data.Maybe

main :: IO ()
main = do 
  xs <- lines <$> readFile "../resources/dat18/day01.txt" 
  putStrLn "part 1:" 
  print $ foldr (+) 0 $ map parse xs -- pt 1
  putStrLn "part 2"
  print $ fromJust $ day01b $ map parse  xs

parse :: String -> Int
parse (x:xs) 
  | x == '+' = read xs
  | x == '-' = -1 * read xs
  | otherwise = 0




-- github user mstksg solution
firstRepeated :: [Int] -> Maybe Int
firstRepeated = go S.empty
  where
    go seen (x:xs)
      | x `S.member` seen = Just x                      -- this is it, chief
      | otherwise         = go (x `S.insert` seen) xs   -- we have to look furhter

day01b :: [Int] -> Maybe Int
day01b = pt2 . scanl (+) 0 . cycle
