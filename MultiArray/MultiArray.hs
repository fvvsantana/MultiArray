

-- Data type definition
data MultiArray a = Elem a | Coll [MultiArray a]

-- Make the MultiArray showable
instance (Show a) => Show (MultiArray a) where
    show (Elem n) = show n
    show (Coll list) = show list

--
instance (Num a) => Num (MultiArray a) where
    Elem a + Elem b = Elem $ a+b
    Coll [] + Coll [] = Coll []
    Coll (a:as) + Coll (b:bs) = insert (a+b) (Coll as + Coll bs)



-- Coll [Elem 0, Elem 1]
-- Coll [ Coll [Elem 0, Elem 1], Coll [Elem 0, Elem 1]]
-- Elem 0 : Coll [Elem 1, Elem 2] = Coll [Elem 0, Elem 1, Elem 2]
-- Coll [Elem 0, Elem 1] + Coll [Elem 2, Elem 3] =
-- (Elem 0 + Elem 2) : (Coll [Elem 1] + Coll [Elem 3])
--                     (Elem 1 + Elem 3) : (Coll [] + Coll [])
--                                       : Coll []
-- Elem (0+2)

-- Coll [Elem (0+2), Elem (1+3)] =
-- Coll [Elem 2, Elem 4]

{-  The type constructor and value constructor for list is "[]".
Therefore, this line wouldn't work:
data MultiArray a = Elem a | Coll List (MultiArray a)
Neither this would work:
bar :: List a -> List a
bar [] = []
-}

-- Constructors

-- Return an empty collection multi array
empty :: MultiArray a
empty = Coll []

-- Insert a element or collection in the beginning of collection
insert :: MultiArray a -> MultiArray a -> MultiArray a
insert x (Coll list) = Coll $ x : list

-- Return a multi array with the dimensions specified in the list filled with 0
-- in every position.
{-
zeros :: (Eq a, Num a, Num b) => [a] -> MultiArray b
zeros [] = Elem 0
zeros (0:_) = empty
zeros (x:xs) = insert (zeros xs) (zeros $ (x-1):xs)
-}
zeros :: (Eq dim, Num dim, Num elem) => [dim] -> MultiArray elem
zeros dimensions = fromFunction dimensions (\pos -> 0)

-- First parameter is a list of dimensions of the multi array to be built.
-- Second parameter is a list of indices that indicates the position of the
-- first element of the multi array to be built.
-- The Third parameter is a functions that receives a list that represents the
-- position of the element to be inserted and returns its value.
-- Return a multi array with specified dimensions, filled with the function f
build :: (Num dim, Eq dim, Num pos) => [dim] -> [pos] -> ([pos] -> elem) -> MultiArray elem
build [] (p:ps) f = Elem (f ps)
build (0:_) _ _ = Coll []
build (d:ds) pos@(p:ps) f = insert (build ds (0:pos) f) $ build ((d-1):ds) ((p+1):ps) f

-- Return a multi array with the dimensions specified in the list. The values of
-- the cells are decided by the function f that receives the position where the
-- element will be inserted and returns the value that will be inserted in that
-- position.
fromFunction :: (Num dim, Eq dim, Num pos) => [dim] -> ([pos] -> elem) -> MultiArray elem
fromFunction dimensions f = build dimensions [0] f

-- Return 1 if all numbers in the list are equal. Else, return 0
idFunction :: (Num a, Eq a, Num b) => [a] -> b
idFunction pos = fromIntegral . fromEnum . allTheSame $ pos
    where
        allTheSame :: (Num a, Eq a) => [a] -> Bool
        allTheSame (p:ps) = and $ map (== p) ps


testSum :: (Num a) => MultiArray a
testSum = arrA + arrB
    where
        arrA = fromFunction [3,3] idFunction
        arrB = fromFunction [3,3] idFunction

main = do
    -- print $ zeros [2,2,2]
    -- print $ idFunction [0,5,0]
    -- print (Elem 0)
    --print (Elem 0)
    print $ testSum

    return 0
