{- Monads -}

data Value t = Value t | NoValue deriving (Show, Eq)

-- the return function for our monad
myreturn x = Value x

-- the bind function for our monad
-- if the monad is NoValue, the result is NoValue, if it is a value, the result
-- applies the function to the monad to produce a new monad
mybind:: (Value t) -> (t -> Value t1) -> Value t1 -- :: is a type declaration, declares the type of the function
-- uses currying notation
mybind (Value x) f = f x
mybind NoValue _ = NoValue -- _ stands for anything, basically a wildcard for parameters
-- eg mybind (Value 10) (\x -> myreturn (2 * x)) returns Value 20
-- sidenote: surrounding a function call name with `` makes it infix

-- create a divide operator that divides 2 Monads, returns NoValue if attempt to divide by 0
(//) vx vy = -- // is just the function name
  vx `mybind` (\x -> vy `mybind` (\y -> if y == 0 then NoValue else myreturn (x / y)))

-- create an addition operator

(+++) vx vy =
  vx `mybind` (\x -> vy `mybind` (\y -> myreturn (x + y)))

-- applies a function on 2 Monads
vapp vx f vy =
  vx `mybind` (\x -> vy `mybind` (\y -> myreturn (f x y)))

-- square root function where sqrt of negative is NoValue
root vx =
  vx `mybind` (\x -> if x < 0 then NoValue else myreturn (sqrt x))

{- Haskell has built in Monads
data Maybe t = Just t | Nothing
the return function is "return"
the bind function is >>=
-}

-- (Just 10) ++++ (Just 20) returns Just 30
(++++) mx my =
  mx >>= (\x -> my >>= (\y -> return (x + y)))

-- divide using built in Maybe monad
(///) mx my =
  mx >>= (\x -> my >>= (\y -> if y == 0 then Nothing else return (x / y)))

-- vapp using built in Maybe Monad
mapp mx f my =
  mx >>= (\x -> my >>= (\y -> return (f x y)))

-- do is a syntax shortcut for the bind
-- binds in order, so can use result of previous binds for the later ones
mapp2 mx f my = do
  x <- mx
  y <- my
  return (f x y)

-- NOTE: lists are also monads, so we can use the bind function on them
-- [1..5] >> = (\x -> [x,x]) duplicates the list [1..5])

-- root with do
root2 mx = do
  x <- mx
  if x < 0 then Nothing else return (sqrt x)
