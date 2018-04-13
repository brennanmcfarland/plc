-- haskell has strict types
-- ie, can't have an array of numbers and numbers of arrays, only one or the other
-- have to create a type for it, for example for tree type
data Tree t = Leaf t | Internal t [Tree t] [Tree t] deriving (Show)

-- also a tree, but restricted to a branching factor of 2
-- doesn't use arrays but explicitly has 2 children
data Tree2 t = Leaf2 t | Internal2 t (Tree2 t) (Tree2 t) deriving (Show)

-- inorder traversal
inorder (Leaf a) = [a]
-- ++ is the built-in append function
inorder (Internal a [l] [r]) = (inorder l) ++ (a : (inorder r))

-- another way to do inorder traversal
inorder2 =
  \t ->
   case t of
     Leaf a -> [a]
     Internal a [l] [r] -> (inorder2 l) ++ (a : (inorder2 r))

-- preorder traversal
preorder2 (Leaf2 a) = [a]
preorder2 (Internal2 a l r) = (a : (preorder2 l)) ++ (preorder2 r)

-- applyinorder takes a tree and function and produces a new tree that applies
-- the function to each node of the tree
applyinorder (Leaf2 a) f = Leaf2 (f a)
applyinorder (Internal2 a l r) f = Internal2 (f a) (applyinorder l f) (applyinorder r f)

-- foldinorder takes a tree, a function and an initial value
-- foldinorder t (:) [] -> [0,2,1,4,5,6,8,9,10]
-- foldinorder t (+) 0 -> 45
-- think like cps (but not passing anything)
foldinorder (Leaf2 a) f i = (f a i)
foldinorder (Internal2 a l r) f i = (foldinorder l f (f a (foldinorder r f i)))
