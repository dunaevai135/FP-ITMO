-- module MyHashMapModule where
import Data.Monoid
import Data.Bits
import Data.Array
import Data.Char

type ElementType = String
type KeyType = String

myHashMapSize :: Int
myHashMapSize = 1000

data MyHashMapElement = MyHashMapElement {
    key :: KeyType,
    value :: ElementType
}

type MyHashMapBucket = [MyHashMapElement]

data MyHashMap = MyHashMap {
    len :: Int,
    elemts :: Array Int MyHashMapBucket
}

insertWithKey :: MyHashMap -> MyHashMapElement -> MyHashMap
insertWithKey hm e
    | length ((elemts hm) ! (myHash $ key e)) == 0 = MyHashMap (len hm) ((elemts hm) // [((myHash $ key e), [e])])
    | otherwise = hm -- TODO add collision resolution

myHash :: KeyType -> Int
myHash e =  (foldl (+) 0 $ map ord e) `rem` myHashMapSize -- TODO use normal hash

insert :: MyHashMap -> KeyType -> ElementType -> MyHashMap
insert hm k v = insertWithKey hm (MyHashMapElement k v)

find :: MyHashMap -> KeyType -> Maybe ElementType
find hm k = val
    where
        val = case bucket of
              [] -> Nothing
              xs -> findElementInBucket k xs
        bucket = (elemts hm) ! (myHash $ k)

findElementInBucket :: KeyType -> MyHashMapBucket -> Maybe ElementType
findElementInBucket k xs = Just value
  where
    (MyHashMapElement key value) = head $ filter (\(MyHashMapElement bucketKey value) -> bucketKey == k) xs

remove :: MyHashMap -> KeyType -> MyHashMap
remove hm k = hm'
    where
        hm' = case bucket of
              [] -> hm
              xs -> MyHashMap (len hm) ((elemts hm) // [((myHash $ k), filter (\(MyHashMapElement bucketKey value) -> bucketKey /= k) xs )])
        bucket = (elemts hm) ! (myHash $ k)

myFold :: (b -> ElementType -> b) -> b -> MyHashMap -> b
myFold f b hm = foldl f b ( map value ( foldl (++) [] (elems(elemts hm)) ))

myMap :: (ElementType -> b) -> MyHashMap -> [b]
myMap f hm = map f ( map value ( foldl (++) [] (elems(elemts hm)) ))

lowerString str = [ toLower loweredString | loweredString <- str]

main :: IO ()
main = do
    -- let a = MyHashMap 10 (listArray (0, 10) (take 10 (repeat ([MyHashMapElement 5 "A"]))))
    let e1 = MyHashMapElement "a" "A"
        e2 = MyHashMapElement "b" "B"
        e3 = MyHashMapElement "c" "B"
        a = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
        a1 = insertWithKey a e1
        a2 = insertWithKey a1 e2
        a25 =remove a2 "b"
        a3 = insertWithKey a25 e3
    print $ value (( (elemts a3) ! myHash "c") !! 0 )
    print $ myHash "c"
    print $ find a3 "a"
    print $ myFold (++) "" a3
    print $ myMap lowerString a3
     
