sumTillNegative :: [Int] -> Int
sumTillNegative =
    go 0
  where
    go total rest =
      case rest of
        [] -> total
        x:xs
          | x < 0     -> total
          | otherwise -> go (total + x) xs
