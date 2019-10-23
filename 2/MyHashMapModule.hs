module MyHashMapModule where
import Data.Monoid
import Data.Bits
import Data.Array
import Data.Char
import Data.Hashable

myHashMapSize :: Int
myHashMapSize = 1000

data MyHashMapElement k v = MyHashMapElement {
    key :: k,
    value :: v
}

type MyHashMapBucket k v = [MyHashMapElement k v]

data MyHashMap k v = MyHashMap {
    len :: Int,
    elemts :: Array Int (MyHashMapBucket k v)
} 


insertWithKey :: (Eq a, Hashable a) => MyHashMap a b -> MyHashMapElement a b -> MyHashMap a b
insertWithKey hm e = MyHashMap (len hm) $ (elemts hm) // [((myHash $ key e), ((elemts hm) ! (myHash $ key e)) ++ [e])]

myHash :: (Eq a, Hashable a) => a -> Int
myHash e =  (abs $ hash e) `rem` myHashMapSize

insert :: (Eq a, Hashable a) => MyHashMap a b -> a -> b -> MyHashMap a b
insert hm k v = insertWithKey hm (MyHashMapElement k v)

find :: (Eq a, Hashable a) => MyHashMap a b -> a -> Maybe b
find hm k = val
    where
        val = case bucket of
              [] -> Nothing
              xs -> findElementInBucket k xs
        bucket = (elemts hm) ! (myHash $ k)

findElementInBucket :: (Eq a, Hashable a) => a -> MyHashMapBucket a b -> Maybe b
findElementInBucket k xs = Just value
  where
    (MyHashMapElement key value) = head $ filter (\(MyHashMapElement bucketKey value) -> bucketKey == k) xs

remove :: (Eq a, Hashable a) => MyHashMap  a b-> a -> MyHashMap a b
remove hm k = hm'
    where
        hm' = case bucket of
              [] -> hm
              xs -> MyHashMap (len hm) $ (elemts hm) // [((myHash $ k), filter (\(MyHashMapElement bucketKey value) -> bucketKey /= k) xs )]
        bucket = (elemts hm) ! (myHash $ k)

myFold :: (Eq a, Hashable a) => (c -> b -> c) -> c -> MyHashMap a b -> c
myFold f b hm = foldl f b $ map value $ foldl (++) [] $ elems $ elemts hm

myMap :: (Eq a, Hashable a) => (b -> c) -> MyHashMap  a b -> [c]
myMap f hm = map f $ map value $ foldl (++) [] $ elems $ elemts hm

myAppend :: (Eq a, Hashable a) => MyHashMap  a b -> MyHashMap  a b -> MyHashMap  a b
myAppend l r = MyHashMap (len l + len r) (listArray (0,(len l + len r)) $ (elems $ elemts l) ++ (elems $ elemts r) )

instance (Eq a, Hashable a) => Monoid (MyHashMap a b) where
    mempty = MyHashMap 0 (listArray (0,-1) ([]))
    mappend = myAppend

-- instance (Eq a, Hashable a) => Eq (MyHashMap a b) where
--     x == y = elemts x == elemts y

lowerString str = [ toLower loweredString | loweredString <- str]

-- main :: IO ()
-- main = do
--     let a = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
--         a1 = insert a "a" "A"
--         a2 = insert a1 "b" "B"
--         a25 =remove a2 "b"
--         -- a3 = insert a25 "c" "B"
--         b = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
--         b1 = insert b "z" "X"
--         a3 = mconcat [a25, b1]
--     print $ myHash "c"
--     print $ (Just (value (( (elemts a3) ! myHash "c") !! 0 ))) == (find a3 "c")
--     print $ find a3 "a"
--     print $ myFold (++) "" a3
--     print $ myMap lowerString a3
