import Test.QuickCheck
import MyHashMapModule
import Data.Array

prop_insert :: String -> String -> Bool
prop_insert key val = res
    where
        res = Just val == find hm' key
        hm = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
        hm' = insert hm key val

prop_remove :: String -> String -> Bool
prop_remove key val = res
    where
        res = Nothing == find a2 key
        a = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
        a1 = insert a key val
        a2 =remove a1 key

prop_foldl :: String -> String -> Bool
prop_foldl key val = res
    where
        res = val == myFold (++) "" a1
        a = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
        a1 = insert a key val

prop_mon :: String -> String -> Bool
prop_mon key val = res
    where
        res = (a1 `mappend` mempty) == a1
        a = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
        a1 = insert a key val

-- prop_mon2 :: String -> String -> Bool
-- prop_mon2 key val = res
--     where
--         res = (a `mappend` b) `mappend` c == a `mappend` (b `mappend` c)
--         a = MyHashMap myHashMapSize (listArray (0,myHashMapSize) (repeat []))
--         a1 = insert a key val

main = do
    quickCheck $ prop_insert
    quickCheck $ prop_remove
    quickCheck $ prop_foldl
    quickCheck $ prop_mon

