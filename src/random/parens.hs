-- beginning code from an interesting Edward Kmett talk! 
-- Check it out
-- https://www.youtube.com/watch?v=Txf7swrcLYs

data B = B !Int !Int
  deriving (Show, Eq) 

instance Semigroup B where
  (<>) (B a b) (B c d)
    | b <= c = B (a + c - b) d 
    | otherwise = B a (d + b - c)

instance Monoid B where
  mempty = B 0 0 

parse '(' = B 0 1
parse ')' = B 1 0
parse  _  = B 0 0


balanced xs = foldMap parse xs == B 0 0
