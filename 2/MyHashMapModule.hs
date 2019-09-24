-- module MyHashMapModule where
import Data.Monoid
import Data.Bits
import Data.Array

data MyHashMapElement = MyHashMapElement {
    key :: Integer,
    value :: String
}

type MyHashMapBucket = [MyHashMapElement]

data MyHashMap = MyHashMap {
    len :: Integer,
    elem :: [MyHashMapBucket]
}

my_hash :: Integer -> Integer
my_hash value = abs((value * 47) `xor` (value * 31)) `rem` 200

main :: IO ()
main = do
    let a = MyHashMap 10 (take 10 (repeat ([MyHashMapElement 5 "A"])))
    print ("asd")
    
