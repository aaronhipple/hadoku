module Utilities
    ( chunk
    ) where

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk length xs =
  let
    (x, remainder) = splitAt length xs
  in
    x : chunk length remainder
    
