import MyHashMapModule
import Data.Array

main :: IO ()
main = do
    let a = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
        a1 = insert a "a" "A"
        a2 = insert a1 "b" "B"
        a25 =remove a2 "b"
        -- a3 = insert a25 "c" "B"
        b = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
        b1 = insert b "z" "X"
        a3 = mconcat [a25, b1]
    print $ myHash "c"
    print $ (Just (value (( (elemts a3) ! myHash "c") !! 0 ))) == (find a3 "c")
    print $ find a3 "a"
    print $ myFold (++) "" a3
    print $ myMap lowerString a3
