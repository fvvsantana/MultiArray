
-- Data type definition
data MultiArray a = Elem a | Coll [MultiArray a]

{-  The value constructor for list is "[]". The type constructor is "List".
Therefore, this line wouldn't work:
data MultiArray a = Elem a | Coll List (MultiArray a)
However, this would work:
bar :: List a -> List a
bar [] = []
-}





main = do
    return 0
