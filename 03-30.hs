{- this is a comment -}
factorial1 n =
  if n == 0
    then
      1
    else
      n * factorial1 (n - 1)

{- ghci is the bash command to run the REPL -}
{- :load filename to load/refresh in the interpreter -}

{- can define the same function twice for different inputs -}
factorial2 0 = 1
factorial2 n = n * factorial2 (n - 1)

{- \n is a lambda -}
{- parenthese are NOT used for function calling, they are purely for grouping,
that's why you can write function calls with the parentheses in different places -}
factorial3 =
  \n -> if n == 0
          then
            1
          else
            n * factorial3 (n - 1)

{- note: haskell is whitespace sensitive at least in terms of grouping things by indentation -}

{- would call with myappend [1,2,3] [4,5,6,7] -}
{- could also do a list range from x to y with [x..y] -}
myappend l1 l2 =
  if l1 == []
    then
      l2
    else
      {- (cons (car l1) (myappend (cdr l1) l2)) -}
      {- car is head, cdr is tail, and cons is : -}
      (head l1) : (myappend (tail l1) l2)

myreverse [] = []
myreverse l = myappend (myreverse (tail l)) ((head l) : [])

{- . composes 2 functions, this version accomplishes the same as above -}
myreverse2 [] = []
myreverse2 l = myappend ((myreverse2 . tail) l) ((head l) : [])

{- replaceall 1 2 [1, 3, 1, 4] -> [2, 3, 2, 4] -}
replaceall a b lis =
  if lis == []
    then
      []
    else
      if (head lis) == a
        then
          b : replaceall a b (tail lis)
        else
          (head lis) : replaceall a b (tail lis)

{- merge [1,3,5] [2,6,7] -> [1,2,3,5,6,7] -}

{- note: can prefix any function with parentheses
ie (:) a b is the same as a : b
can also infix with ``
ie (a 'func' b) is the same as (func a b) -}
