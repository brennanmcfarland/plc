{- factorial 3 ways -}
factorial n =
  if n == 0
    then
      1
    else
      n * factorial (n - 1)

factorial2 0 = 1
factorial2 n = n * factorial2 (n - 1)

factorial3 =
  \n -> if n == 0
    then
      1
    else
      n * factorial3 (n - 1)

{- cps style -}
{- call with fact_cps 10 (\v ->v) -}
fact_cps 0 return = return 1
fact_cps n return = fact_cps (n-1) (\v -> return (n*v))

{- merge_cps -}
merge_cps [] l2 return = return l2
merge_cps l1 [] return = return l1
merge_cps l1 l2 return =
  if ((head l1) < (head l2))
    then
      merge_cps (tail l1) l2 (\v -> return ((head l1) : v))
    else
      merge_cps l1 (tail l2) (\v -> return ((head l2) : v))

{- split_cps -}
{- call with split_cps [1..21] (\a b -> [a,b]) -}
split_cps [] return = return [] []
split_cps [a] return = return [a] [] {- [a] is a list with 1 element which we labelled a -}
split_cps lis return =
  split_cps ((tail . tail) lis) (\v1 v2 -> return ((head lis) : v1) (((head . tail) lis) : v2))
{- let f = split_cps [1..10] stores the function in f, can call later with different input values,
 can specify only some and not all -}

{- mergesort_cps -}
mergesort_cps [] return = return []
mergesort_cps [a] return = return [a]
mergesort_cps lis return =
  split_cps lis (\a b -> mergesort_cps a (\c -> mergesort_cps b (\d -> merge_cps c d return)))

{- TYPES -}
{- create a new Coordinate Type that is a subtype of the Show type, which allows it to be printed,
 can have 2 or 3 doubles -}
data Coordinate = Coord2 Double Double | Coord3 Double Double Double deriving (Show)

getx (Coord2 x y) = x
gety (Coord2 x y) = y
{- can call with distance (Coord2 1 2) (Coord2 10 20), only works for Coord2s though -}
distance (Coord2 a b) (Coord2 c d) = sqrt((a - c)*(a - c) + (b - d) * (b - d))
{- can work on either Coord2 or Coord3s, finds distance of x,y projection using generic types -}
distance2 c1 c2 = sqrt((getx c1 - getx c2) * (getx c1 - getx c2) + (gety c1 - gety c2) * (gety c1 - gety c2))
