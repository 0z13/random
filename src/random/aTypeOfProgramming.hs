-- Notes: A type of proramming
-- Dos.  
-- Isomorphic Tuples 
--

f1 :: (z, (x, y)) ->
  ((x,y), z)
f1 (z,(x,y)) = ((x,y), z)

-- Representation
-- Succ and a conversion from Nat to int

data Nat = Zero | Succ Nat
  deriving Show

one = Succ Zero
two = Succ one
three = Succ two 

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Succ y) = 1 + fromNat y

-- From this example the author discusses the halting problem
-- The user must get recursion right, because it is impossible
-- to determine programmatically whether a program halts or not.


-- We learned too much, let's fous on the essentials
-- Lambda calculus.

data Expr 
  = Lam String Expr
  | App Expr Expr
  | Var String

-- Here's the identitiy function
-- (\x -> x)
exprId :: Expr
exprId = Lam "x" (Var "x")

-- We are magically gifted numbers
-- (\x -> "five")
lambdaFive :: Expr
lambdaFive = App exprId (Var "five")


-- Lists, maps etc.
-- 1 : 2 : 3 : [] == [1,2,3]

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = (f x) : myMap f xs


-- Eta-conversion, pointfree notation, partial application 
-- Then functors. Maps shouldn't be abritrarily restricted 
-- my functor is a finktor
class Finktor f where
  finkmap :: (a -> b) -> f a -> f b

instance Finktor [] where
  finkmap f []     = []
  finkmap f (x:xs) = f x : finkmap f xs

-- We want a map that we can't implement by just 
-- returning the empty list.
-- We are not there yet.
-- But atleast we can do this now...

--g :: Finctor f => f a -> f a
--g xs = finkmap (\x -> x + 1)  xs

-- He rants about parametric polymorphism,
-- tomorrow we will cover the functor laws.
-- Functor law involves identity so:

-- fmap id x == id x --- first functor law

-- composition fmap (compose g f) x == fmap g fmap f x)

-- an example of composition:

composition :: (Int -> Int) -> (Int -> Int) -> Int -> Int
composition f g x = f (g x) 


-- Fixity
-- Kinda of noisy. This works because ||| takes precedence
-- over +, plus having a precendence of 6.
-- If i add parenthesis it wont work

(|||) :: Int -> Int -> Int
(|||) x y = case y of 
  0 -> 0
  _ -> x + x ||| (y - 1)

infix 7 ||| 






