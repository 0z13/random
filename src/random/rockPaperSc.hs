-- From the stanford slides.
--
data Move = Rock | Paper | Scissors
  deriving (Eq, Read, Show, Bounded) 

data Outcome  = Lose | Tie | Win deriving (Show,  Eq, Ord)

outcome :: Move -> Move -> Outcome
outcome Rock Scissors = Win 
outcome Paper  Rock = Win 
outcome Scissors Paper = Win 
outcome x y 
  | x == y = Tie
  | otherwise = Lose

parseMove :: String -> Maybe Move
parseMove x
  | x == "Rock"     = Just Rock 
  | x == "Scissors" = Just Scissors
  | x == "Paper"    = Just Paper
  | otherwise       = Nothing

getMove :: IO Move
getMove = do
  putStrLn "Please make a move"
  x <- getLine 
  case parseMove x of 
    Just x  -> return x
    Nothing -> getMove

game :: Move -> IO ()
game x = do
  y <- getMove
  putStrLn $ show $ outcome x y

