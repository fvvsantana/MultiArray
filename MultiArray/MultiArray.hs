{-  The type constructor and value constructor for list is "[]".
Therefore, this line wouldn't work:
data MultiArray a = Elem a | Coll List (MultiArray a)
Neither this would work:
bar :: List a -> List a
bar [] = []
-}

-- Data type definition
data MultiArray a = Elem a | Coll [MultiArray a]

-- Make the MultiArray showable
instance (Show a) => Show (MultiArray a) where
    show (Elem n) = show n
    show (Coll list) = show list

-- Operator overloading
instance (Num a) => Num (MultiArray a) where
    -- Addition
    (+) (Elem a) (Elem b) = Elem $ a+b
    (+) (Coll []) (Coll []) = Coll []
    (+) (Coll (a:as)) (Coll (b:bs)) = insert (a+b) (Coll as + Coll bs)

    -- Subtraction
    (-) (Elem a) (Elem b) = Elem $ a-b
    (-) (Coll []) (Coll []) = Coll []
    (-) (Coll (a:as)) (Coll (b:bs)) = insert (a-b) (Coll as - Coll bs)

    -- Multiplication
    (*) (Elem a) (Elem b) = Elem $ a*b
    (*) (Coll []) (Coll []) = Coll []
    (*) (Coll (a:as)) (Coll (b:bs)) = insert (a*b) (Coll as * Coll bs)

    -- Abs
    abs (Elem a) = Elem (abs a)
    abs (Coll []) = Coll []
    abs (Coll (a:as)) = insert (abs a) (abs (Coll as))

    -- Signum
    signum (Elem a) = Elem (signum a)
    signum (Coll []) = Coll []
    signum (Coll (a:as)) = insert (signum a) (signum (Coll as))

    -- fromInteger
    fromInteger i = Elem (fromInteger i)


-- Constructors

-- Return an empty collection multi array
empty :: MultiArray a
empty = Coll []

-- Insert a element or collection in the beginning of collection
insert :: MultiArray a -> MultiArray a -> MultiArray a
insert x (Coll list) = Coll $ x : list

-- Return a multi array with the dimensions specified in the list filled with 0
-- in every position.
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

main = do
    let dimensions = [3,3]
    let a = fromFunction dimensions idFunction
    --let b = fromFunction dimensions (\pos -> 2 * (idFunction pos))
    let b = fromFunction dimensions ((*2) . idFunction)
    putStrLn $ (show a) ++ " + " ++ (show b) ++ " = " ++ (show (a+b))

    --print $ zeros [2,2,2]
    --print $ idFunction [0,5,0]
    --print (Elem 0)
    --print (Elem 0)
    --print (fromFunction [2,2] negIdFunction)
    --print $ testAddition
    --print $ testSubtraction
    --print $ testMultiplication
    --print testAbs
    --print testSignum
    --print . signum $ (fromFunction [3,3] (\x -> -5))
    --print . fromInteger $ 5
    return 0

-- Test functions

testAddition :: (Num a) => MultiArray a
testAddition = arrA + arrB
    where
        arrA = fromFunction [3,3] idFunction
        arrB = fromFunction [3,3] idFunction

testSubtraction :: (Num a) => MultiArray a
testSubtraction = arrA - arrB
    where
        arrA = fromFunction [3,3] idFunction
        arrB = fromFunction [3,3] idFunction

testMultiplication :: (Num a) => MultiArray a
testMultiplication = arrA * arrB
    where
        arrA = fromFunction [3,3] idFunction
        arrB = fromFunction [3,3] idFunction


-- Return 1 if all numbers in the list are equal. Else, return 0
negIdFunction :: (Num a, Eq a, Num b) => [a] -> b
negIdFunction pos = - (idFunction pos)

testAbs :: (Num a) => MultiArray a
testAbs = abs (fromFunction [3,3] negIdFunction)

testSignum :: (Num a) => MultiArray a
testSignum = signum (fromFunction [3,3] negIdFunction)
