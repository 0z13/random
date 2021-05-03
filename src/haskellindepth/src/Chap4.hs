{-# LANGUAGE InstanceSigs #-}

module Chap4 where
import Control.Monad.State
import Data.Maybe
import qualified Text.Read as T
-- Pracical monads.
--
--
-- Maybe, error handler


data Sumthing a  = Za a | Nuthin 
  deriving (Show)


instance Functor Sumthing where
  fmap  :: (a -> b) -> Sumthing a -> Sumthing b
  fmap f (Za x) = Za (f x) 
  fmap f Nuthin = Nuthin    

readPerhaps :: String -> Maybe Int
readPerhaps xs = case T.readMaybe xs of
  (Just x) -> Just x
  Nothing  -> Nothing 

multipleStr  :: String -> Maybe Int
multipleStr x = (*2) <$> readPerhaps x

-- Not really using monads there, hm
--
--
--
-- State 
-- For passing around variables?

addones :: Int -> State Int ()
addones n = do
  s <- get
  put (s + n)

add1 :: Int -> State Int ()
add1 n = modify' (+n)


-- Rock Paper Scissors
--
test :: State Int Int
test = do 
  modify (+1)
  get

main :: IO ()
main = print $ execState test 100

