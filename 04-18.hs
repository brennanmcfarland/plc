-- higher order functions

foldleft f i [] = i
foldleft f i (h:t) = foldleft f (f i h) t

foldright f i [] = i
foldright f i (h:t) = f h (foldright f i t)

-- :t function tells you the type of function
-- haskell has built in functions foldl and foldr

-- map, equivalent to built in map
mymap f [] = []
mymap f (h:t) =
  (f h) : (mymap f t)

-- filter
-- eg myfilter (\x -> x > 10) [5,29,16,7,35] -> [29,16,35]
myfilter f [] = []
myfilter f (h:t) =
  if (f h)
    then h : (myfilter f t)
  else
    myfilter f t

-- flip
flip f = (\x y -> f y x)

-- dot product
dotproduct v1 v2 = foldl (+) 0 (applyit (*) v1 v2)

-- reverse
myreverse l = foldl (flipit (:)) [] l

-- quicksort
qsort [] = []
qsort (h:t) = qsort (filter (\x -> x <= h) t) ++ (h: (qsort (filter (\x -> x > h) t)))
