threeLetters :: String -> Bool
threeLetters xs = solve xs 0
  where solve [] counter 
          | counter == 3 = True
          | otherwise    = False
        solve (x:xs) counter = solve xs (counter + 1)


threeLetters2 :: String -> Bool
threeLetters2 = (\x -> x == 3) . length

