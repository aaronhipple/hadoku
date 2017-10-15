module Lib
  ( someFunc
  , Puzzle(..)
  , Cell(..)
  , Value(..)
  , toString
  , samplePuzzle
  , countEmpties
  ) where

import           Data.List
import           Utilities ()

someFunc :: IO ()
someFunc = putStrLn $ toString samplePuzzle

data Value
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded)

data Cell
  = Empty
  | Val Value
  deriving (Eq)

newtype Puzzle =
  Puzzle [[Cell]]

samplePuzzle :: Puzzle
samplePuzzle =
  Puzzle
    [ [Val Five, Val Three, Empty, Empty, Val Seven, Empty, Empty, Empty, Empty]
    , [Val Six, Empty, Empty, Val One, Val Nine, Val Five, Empty, Empty, Empty]
    , [Empty, Val Nine, Val Eight, Empty, Empty, Empty, Empty, Val Six, Empty]
    , [Val Eight, Empty, Empty, Empty, Val Six, Empty, Empty, Empty, Val Three]
    , [ Val Four
      , Empty
      , Empty
      , Val Eight
      , Empty
      , Val Three
      , Empty
      , Empty
      , Val One
      ]
    , [Val Seven, Empty, Empty, Empty, Val Two, Empty, Empty, Empty, Val Six]
    , [Empty, Val Six, Empty, Empty, Empty, Empty, Val Two, Val Eight, Empty]
    , [Empty, Empty, Empty, Val Four, Val One, Val Nine, Empty, Empty, Val Five]
    , [Empty, Empty, Empty, Empty, Val Eight, Empty, Empty, Val Seven, Val Nine]
    ]

countEmpties :: Puzzle -> Int
countEmpties (Puzzle rows) =
  let findEmpties = filter (Empty ==)
  in length $ concatMap findEmpties rows

-- |Generate a list of possible puzzle solutions.
-- puzzleSolutions :: Puzzle -> [Puzzle]
-- puzzleSolutions puzzle =
--   let
--     (Puzzle rows) = puzzle
--     emptyLength = countEmpties puzzle
--     perms = mapM (const [(minBound :: Value) ..]) [1..emptyLength]
--     init = ([], perms)
--     groups = group $ concat rows
--
--  in
--    -- TODO
--    []
-- |Check if a puzzle is in a valid configuration.
isValid :: Puzzle -> Bool
isValid (Puzzle rows) =
  let cols = transpose rows
      boxes = makeBoxes rows
  in all listValid cols && all listValid rows && all listValid boxes

allVals :: [Value]
allVals = [(minBound :: Value) ..]

-- |Make a list of boxes for the given puzzle
makeBoxes :: [[Cell]] -> [[Cell]]
makeBoxes rows =
  let getThreeAt :: Int -> [a] -> [a]
      getThreeAt x list = take 3 $ snd $ splitAt x list
      boxAt :: [[Cell]] -> (Int, Int) -> [Cell]
      boxAt rs (x, y) = concat $ getThreeAt x $ map (getThreeAt y) rs
      toTuples :: [a] -> [(a, a)]
      toTuples (x:y:xs) = (x, y) : toTuples xs
      toTuples []       = []
      allCoords :: [(Int, Int)]
      allCoords = toTuples $ concat $ mapM (const [0, 3, 6]) ([1 .. 2] :: [Int])
  in map (boxAt rows) allCoords

-- |Check if a list of Cells (a row, a column, or a block) is valid
listValid :: [Cell] -> Bool
listValid xs =
  let filledVals :: [Cell]
      filledVals = map Val allVals
      hasOneOrFewer :: [Cell] -> Cell -> Bool
      hasOneOrFewer list val = length (filter (val ==) list) <= 1
  in all (hasOneOrFewer xs) filledVals

-- |Convert a puzzle to a string for printing.
toString :: Puzzle -> String
toString (Puzzle rows) =
  let rowToString :: [Cell] -> String
      rowToString row = intersperse ' ' $ map toChar row
  in unlines $ map rowToString rows

instance Show Cell where
  show cell = [toChar cell]

instance Show Value where
  show value = [toChar $ Val value]

-- |Convert a cell value to a char for printing.
toChar :: Cell -> Char
toChar Empty       = '.'
toChar (Val One)   = '1'
toChar (Val Two)   = '2'
toChar (Val Three) = '3'
toChar (Val Four)  = '4'
toChar (Val Five)  = '5'
toChar (Val Six)   = '6'
toChar (Val Seven) = '7'
toChar (Val Eight) = '8'
toChar (Val Nine)  = '9'
