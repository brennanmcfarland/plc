-- generic list of elements and sublists
data GList t = Element t | Sublist [GList t] deriving (Show)

-- remove duplicates of GList
gremovedups [] = []
gremovedups [Element a] = [Element a]
gremovedups ((Sublist l):t) = (Sublist (gremovedups l)) : (gremovedups t)
gremovedups ((Element a):(Element b):t) =
  if a == b
    then
      gremovedups ((Element b):t)
    else
      (Element a):(gremovedups ((Element b) :t))
gremovedups ((Element a):t) = (Element a):(gremovedups t)

-- bubbledown
data Tree t = Leaf t | Internal t (Tree t) (Tree t) deriving (Show)

value (Leaf a) = a
value (Internal a l r) = a

setvalue a (Leaf _) = Leaf a
setvalue a (Internal _ l r) = Internal a l r

bubbledown (Leaf a) = (Leaf a)
bubbledown (Internal a l r) =
  if (value r) > (value l) && a > value l
    then
      Internal (value l) (bubbledown (setvalue a l)) r
    else if a > (value r)
      then
        Internal (value r) l (bubbledown (setvalue a r))
    else
      Internal a l r

-- checkcons
checkcons ma ml f = do
  a <- ma
  l <- ml
  if (f a) then return (a:l) else Nothing

--checklist
checklist [] f = Just []
checklist (h:t) f = checkcons (Just h) (checklist t f) f

-- list monad
data MList t = Pair t (MList t) | Null deriving (Show)

lreturn x = Pair x Null

append Null b = b
append (Pair a b) c = Pair a (append b c)

lbind Null _ = Null
lbind (Pair a b) f = append (f a) (lbind b f)
