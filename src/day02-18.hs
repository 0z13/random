{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import Data.Containers.ListUtils
import Data.List
-- pretty sick frequency map 

freqs :: Ord a => [a] -> M.Map a Int
freqs = M.fromListWith (+) . map (,1)

day02a :: Ord a => [[a]] -> [Int]
day02a xs = filter (> 1) $ concatMap (nubOrd . M.elems . freqs) xs


main :: IO ()
main = do
  xs <- lines <$> readFile "../resources/dat18/day02.txt"
  print $ foldr (*) 1 $ map length $ group . sort $ day02a xs
