{-
  A textbook implementation of a hash map using chaining for collisions and
  resizing by creating a new map.
-}
import Data.Char
import Data.Hashable

-- | Keep track of how many buckets are filled, and the buckets themselves.
data HashMap a b = MkHashMap Int (ThinHashMap a b)
-- | Remove any information from the HashMap.
type ThinHashMap a b = [Bucket a b]

{-|
  A Bucket is a list of key-value pairs.
  The keys will have to be anything that's Hashable.
  The values can be anything.
|-}
type Bucket a b = [(a,b)]

growHashMap :: (Eq a, Hashable a) => ThinHashMap a b -> HashMap a b
growHashMap hashMap = hashMap'
  where
    hashMap' = foldl (\hashMap (key,value) ->
                  insertOrUpdateKey hashMap key value
                ) emptyHashMap $ concat hashMap
    emptyHashMap = MkHashMap 0 $ replicate ((length hashMap)^2) []

removeHashMapKey :: (Eq a, Hashable a) => HashMap a b -> a -> HashMap a b
removeHashMapKey (MkHashMap filled hashMap) key = hashMap'
  where
    hashMap'        = MkHashMap (filled-1) $ updateBucketInHashMap hashMap index bucket'
    bucket'         = filter (\(existingKey,_) -> existingKey /= key) bucket
    (bucket,index)  = getBucketAtKey hashMap key

_insertOrUpdateKey :: (Eq a, Hashable a) => ThinHashMap a b -> Int -> Bucket a b -> ThinHashMap a b
_insertOrUpdateKey hashMap index (element@(key',value'):xs) = hashMap'
  where
    hashMap'   = updateBucketInHashMap hashMap index bucket'
    bucket'    = element:uniqueKeys
    uniqueKeys = filter (\(key,_) -> key /= key') xs

{-|
  Add 1 to filled, even if we update. It means our list is too small and we're
  getting lots of collisions. Resizing the list should fix this.
|-}
insertOrUpdateKey :: (Eq a, Hashable a) => HashMap a b -> a -> b -> HashMap a b
insertOrUpdateKey (MkHashMap filled hashMap) key value =
  if filled > length hashMap
    then growHashMap hashMap'
    else MkHashMap (filled+1) hashMap'
  where
    hashMap'       = case bucket of
                        [] -> _insertOrUpdateKey hashMap index [(key,value)]
                        xs -> _insertOrUpdateKey hashMap index ((key,value):xs)
    (bucket,index) = getBucketAtKey hashMap key

lookupHashMapKey :: (Eq a, Hashable a) => ThinHashMap a b -> a -> Maybe b
lookupHashMapKey hashMap key = value
  where
    value  = case bucket of
              [] -> Nothing
              xs -> findElementInBucket key xs
    bucket = hashMap !! index
    index  = getHashMapIndexFromHash hashMap key

updateBucketInHashMap :: (Eq a, Hashable a) => ThinHashMap a b -> Int -> Bucket a b -> ThinHashMap a b
updateBucketInHashMap hashMap index bucket' = hashMap'
  where
    hashMap'               = left ++ [bucket'] ++ right
    (left,(removed:right)) = splitAt index hashMap

getBucketAtKey :: Hashable a => ThinHashMap a b -> a -> (Bucket a b, Int)
getBucketAtKey hashMap key = (bucket,index)
  where
    bucket = hashMap !! index
    index  = getHashMapIndexFromHash hashMap key

getHashMapIndexFromHash :: Hashable a => ThinHashMap a b -> a -> Int
getHashMapIndexFromHash hashMap key = (hash key) `mod` (length hashMap)

findElementInBucket :: (Eq a, Hashable a) => a -> Bucket a b -> Maybe b
findElementInBucket searchKey xs = value
  where
    value = if length listValues > 0
            then let (key,value) = head listValues in Just value
            else Nothing
    listValues = filter (\(bucketKey,value) -> bucketKey == searchKey) xs

main :: IO ()
main = do
  print "Hi"
