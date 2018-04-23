{-
Brennan McFarland
April 16, 2018
Programming Exercise 3
-}

-- 1. removedups takes a list and removes duplicate elements
-- removedups [1,2,2,3,3,3,4,3,4,5,5,5,4,3,3,2,1] => [1,2,3,4,3,4,5,4,3,2,1]
removedups [] = []
removedups [a] = [a]
removedups lis =
  if (head lis) == ((head . tail) lis)
    then
      removedups (tail lis)
    else
      (head lis) : (removedups (tail lis))

-- 2. cps version of removedups
-- removedups_cps [1,2,2,3,3,3,4,3,4,5,5,5,4,3,3,2,1] (\v -> v) => [1,2,3,4,3,4,5,4,3,2,1]
removedups_cps [] return = return []
removedups_cps [a] return = return [a]
removedups_cps lis return =
  if (head lis) == ((head . tail) lis)
    then
      removedups_cps (tail lis) return
    else
      removedups_cps (tail lis) (\v -> return ((head lis) : v))

{- 3. a type that allows us to have nested lists. has two kinds of values, elements and sublists.

example: [Element 1,Element 3,Sublist [Element 4,Sublist [Sublist [Element 5],Sublist []]],Element 6]
-}
data Sublist t = Element t | Sublist [Sublist t] deriving (Show, Eq)

{- 4. gremovedups takes a list containing elements and sublists and returns a list with the same structure,
 but if any "element" is preceded by an identical element, that element is removed.

example: gremovedups [Element 4,Element 4,Element 5,Sublist [Element 6,Element 6,Sublist[Element 8,
 Element 8,Element 8]], Element 5,Element 5] =>
[Element 4,Element 5,Sublist [Element 6,Sublist [Element 8]],Element 5]
-}
gremovedups [] = [] -- empty list
gremovedups [Sublist x] = [Sublist (gremovedups x)] -- one element, a sublist
gremovedups [x] = [x] -- one element, an Element
-- multiple elements, the first is a list
gremovedups l@((Sublist x) : xs) = [Sublist (gremovedups x)] ++ (gremovedups (tail l))
-- multiple elements, the first is an Element
gremovedups l =
  if (head l) == ((head . tail) l)
    then
      gremovedups (tail l)
    else
      (head l) : gremovedups (tail l)

-- binary tree for 5.
data Tree t = Leaf t | Internal t (Tree t) (Tree t) deriving (Show)
get (Leaf v) = v
get (Internal v c1 c2) = v
set (Leaf v) vnew = (Leaf vnew)
set (Internal v c1 c2) vnew = (Internal vnew c1 c2)

{- 5. bubbledown takes a Tree; if the element stored in the root is larger than either children, swap
  the element with the smaller child, and recurse on the child you swapped the element with, until
  either you reach a leaf or when the element of the node is smaller than both its children

examples: bubbledown (Internal 10 (Internal 3 (Internal 6 (Leaf 7) (Leaf 11)) (Internal 4 (Leaf 8) (Leaf 6)))
  (Leaf 12)) => Internal 3 (Internal 4 (Internal 6 (Leaf 7) (Leaf 11)) (Internal 6 (Leaf 8) (Leaf 10))) (Leaf 12)
  bubbledown (Internal 5 (Internal 3 (Internal 6 (Leaf 7) (Leaf 11)) (Internal 4 (Leaf 8) (Leaf 6))) (Leaf 12))
  => Internal 3 (Internal 4 (Internal 6 (Leaf 7) (Leaf 11)) (Internal 5 (Leaf 8) (Leaf 6))) (Leaf 12)
  -}
bubbledown (Leaf a) = (Leaf a)
bubbledown (Internal v c1 c2) =
  if v > (min (get c1) (get c2))
    then swap_smaller (Internal v c1 c2)
  else
    (Internal v c1 c2)

swap_smaller (Internal v c1 c2) =
  if (get c1) > (get c2)
    then (Internal (get c2) c1 (bubbledown (set c2 v)))
  else
    (Internal (get c1) (bubbledown (set c1 v)) c2)

{- 6. checkcons has the type
  checkcons :: Maybe a -> Maybe [a] -> (a -> Bool) -> Maybe [a],
  takes a Maybe value of some type, a Maybe list of the same type (as a monad),
  and a test function and returns a Maybe list of the same type. If either Maybe is Nothing, the result is Nothing.
  If the first Maybe value passes the test function, the result has the first element cons'd onto the front of the list.
  Otherwise the result is Nothing

examples: checkcons (Just 5) (Just [6,7,8]) (\x -> (x > 0))  =>  Just [5,6,7,8]
  checkcons (Just 5) (Just [6,7,8]) (\x -> (x < 0))  =>  Nothing
-}
checkcons :: Maybe a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
checkcons mv ml f = do
  v <- mv
  l <- ml
  if (f v) == True
    then return (v : l)
  else
    Nothing

{- 7. checklist takes a list and a function and returns Nothing if the elements in the list fail to pass the function
  and the list (embedded in a Maybe) if all the elements pass
examples: checklist "aaaaa" (\x -> x == 'a')  =>  Just "aaaaa"
checklist "abcde" (\x -> (x >= 'a' && x <= 'z'))  =>  Just "abcde"
checklist "abcDe" (\x -> (x >= 'a' && x <= 'z'))  =>  Nothing
checklist [1,-2,3] (\x -> x > 0)  =>  Nothing
-}
checklist [] f = (Just [])
checklist l f =
  (checkcons (Just (head l)) (checklist (tail l) f) f)

{- 8. a list monad that generalizes a list
  For example, the following is a valid "list": Pair 4 (Pair 5 (Pair 6 Null))
  also a binding function lbind and a return function lreturn to make a list monad
  example: (Pair 4 (Pair 5 (Pair 6 Null))) `lbind` (\x -> lreturn (2 * x))   =>   Pair 8 (Pair 10 (Pair 12 Null))
-}
data Pair t = Pair t (Pair t) | Null deriving (Show, Eq)
val (Pair t1 t2) = t1
lreturn t = Pair t Null
lbind:: (Pair t1) -> (t1 -> Pair t2) -> Pair t2
lbind Null _ = Null
lbind (Pair t1 Null) f = (f t1)
lbind (Pair t1 t2) f = (Pair (val(f t1)) (lbind t2 f))
