-- import Data.List

-- Data type definition
data MultiArray a = Elem a | Coll [MultiArray a] deriving(Show)
-- Coll [Elem 0, Elem 1]
-- Coll [ Coll [Elem 0, Elem 1], Coll [Elem 0, Elem 1]]
-- Elem 0 : Coll [Elem 1, Elem 2] = Coll [Elem 0, Elem 1, Elem 2]

{-  The type constructor and value constructor for list is "[]".
Therefore, this line wouldn't work:
data MultiArray a = Elem a | Coll List (MultiArray a)
Neither this would work:
bar :: List a -> List a
bar [] = []
-}

-- Constructors

-- Return an empty multi array
empty :: MultiArray a
empty = Coll []

-- Insert a element or collection in the beginning of collection
insert :: MultiArray a -> MultiArray a -> MultiArray a
insert x (Coll list) = Coll (x : list)


main = do
    return 0
