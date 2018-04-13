{- types in Haskell
  data - create new data type
  multiple constructors, can have 0 or 1 or 2 or 3 doubles -}
-- t is a generic type parameter
data Coordinate t = Coord2D t t | Coord3D t t t | Coord1D t | Zero deriving (Show) {- deriving (Show) allows it to be displayed -}
-- creating an instance of Eq for Coordinate here lets us compare different types of coords for equality
instance (Eq t) => Eq (Coordinate t) where
  ()
  (Coord1D x) == (Coord1D y) = x == y
  Zero == Zero = True
--  c1 == c2 = ((getx c1 == getx c2) && (gety c1 == gety c2) && (getz c2 == getz c2))

{- note when calling, must put negatives in parentheses or it would group w/ prev arg -}
uglydistance (Coord2D a b) (Coord2D c d) = sqrt((a-c)*(a-c) + (b-d)*(b-d))
uglydistance (Coord3D a b x) (Coord3D c d y) = sqrt((a-c)*(a-c) + (b-d)*(b-d) + (x-y)*(x-y))

getx Zero = 0
getx (Coord1D x) = x
getx (Coord2D x y) = x
getx (Coord3D x y z) = x
gety Zero = 0
gety (Coord1D x) = 0
gety (Coord2D x y) = y
gety (Coord3D x y z) = y
getz Zero = 0
getz (Coord1D x) = 0
getz (Coord2D x y) = 0
getz (Coord3D x y z) = z

distance c1 c2 = sqrt((getx c1 - getx c2) * (getx c1 - getx c2) + (gety c1 - gety c2) * (gety c1 - gety c2) + (getz c1 - getz c2) * (getz c1 - getz c2))

-- single line comment
{- |+ to add coords, can surround w/ ()s to call as prefix or use regularly as infix -}
-- there's so many functions because it needs to return the widest type
(|+) (Coord3D x y z) c = Coord3D (x + getx c) (y + gety c) (z + getz c)
(|+) c (Coord3D x y z) = Coord3D (x + getx c) (y + gety c) (z + getz c)
(|+) (Coord2D x y) c = Coord2D (x + getx c) (y + gety c)
(|+) c (Coord2D x y) = Coord2D (x + getx c) (y + gety c)
(|+) (Coord1D x) c = Coord1D (x + getx c)
(|+) c (Coord1D x) = Coord1D (x + getx c)
(|+) Zero Zero = Zero
