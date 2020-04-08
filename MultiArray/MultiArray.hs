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
insert x (Coll list) = Coll $ x : list

-- Return a multi array with the dimensions specified in the list filled with 0
-- in every position.
zeros :: (Eq a, Num a, Num b) => [a] -> MultiArray b
zeros [] = Elem 0
zeros (0:_) = empty
zeros (x:xs) = insert (zeros xs) (zeros $ (x-1):xs)



main = do
    print $ zeros [2,2]
    return 0
