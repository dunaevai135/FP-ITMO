import Data.Char

type HashMap a = [Bucket a]
type Bucket a = [(String,a)]

removeHashMapKey :: HashMap a -> String -> HashMap a
removeHashMapKey hashMap key = hashMap'
  where
    hashMap' = left ++ [bucket'] ++ right
    bucket' = filter (\(existingKey,_) -> existingKey /= key) bucket
    (left,(removed:right)) = splitAt index hashMap
    bucket = hashMap !! index
    index = getHashMapIndexFromHash hashMap key

insertOrUpdateHashMapKey :: HashMap a -> String -> a -> HashMap a
insertOrUpdateHashMapKey hashMap key value = hashMap'
  where
    hashMap' = case bucket of
                [] -> insertOrUpdateList hashMap index [(key,value)]
                xs -> insertOrUpdateList hashMap index ((key,value):xs)
    bucket = hashMap !! index
    index = getHashMapIndexFromHash hashMap key

insertOrUpdateList :: HashMap a -> Int -> Bucket a -> HashMap a
insertOrUpdateList hashMap index (element@(key',value'):xs) = hashMap'
  where
    hashMap' = left ++ [bucket'] ++ right
    bucket' = element:uniqueKeys
    uniqueKeys = filter (\(key,_) -> key /= key') xs
    (left,(removed:right)) = splitAt index hashMap

lookupHashMapKey :: HashMap a -> String -> Maybe a
lookupHashMapKey hashMap key = value
  where
    value = case bucket of
              [] -> Nothing
              xs -> Just $ findElementInBucket key xs
    bucket = hashMap !! index
    index = getHashMapIndexFromHash hashMap key

getHashMapIndexFromHash :: HashMap a -> String -> Int
getHashMapIndexFromHash hashMap key = ((calculateHash key) `mod` (length hashMap))

findElementInBucket :: String -> Bucket a -> a
findElementInBucket searchKey xs = value
  where
    (key,value) = head $ filter (\(bucketKey,value) -> bucketKey == searchKey) xs

calculateHash :: String -> Int
calculateHash key = hash
  where
    (hash,acc) = foldl reducer (0,length key) key
    reducer = (\(hash,index) char -> ((hash+(ord char)*31^index), index-1))

main :: IO ()
main = do
  let hm = [[],[],[],[]]
  let h5 = insertOrUpdateHashMapKey hm "hello" 4
  let h6 = removeHashMapKey hm "hello"
  print $ show (h6 :: HashMap Int)

