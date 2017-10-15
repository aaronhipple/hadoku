module Utilities
  ( chunk
  , quicksort
  ) where

import           Data.List

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk size xs =
  let (x, remainder) = splitAt size xs
  in x : chunk size remainder

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
  where
    (lesser, greater) = partition (< p) xs
