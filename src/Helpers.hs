module Helpers where

import qualified Data.Map as M
import           System.Random (randomR, RandomGen)


-- Return the keys of a Map filtered by a predicate on the key
filteredKeys :: (k -> Bool) -> M.Map k a -> [k]
filteredKeys p = filter p . M.keys


-- Generate an infinite list of pseudo-random numbers ranging between 1 and n where n increases by
-- one for each subsequent element in the list
randomIndices :: RandomGen g => g -> Int -> [Int]
randomIndices gen n = let (index, gen') = randomR (1, n) gen
                      in  index : randomIndices gen' (n+1)


minBy :: Ord b => (a -> b) -> (a -> c) -> [a] -> c
minBy measure finalize = go
  where
    go [a]                     = finalize a
    go (a:a':rest)
      | measure a < measure a' = go (a :rest)
      | otherwise              = go (a':rest)
