{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches = curry $ foldl (flip ((+).(.?.))) 0 . uncurry (zipWith (==))
  where (.?.) True  = 1
        (.?.) False = 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (length . ($ xs) . elemIndices) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches = curry $ sum . uncurry (flip ((zipWith min) . countColors) . countColors)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess =
  let em = exactMatches secret guess
      mm = matches      secret guess
      nm = mm - em
  in  Move guess em nm

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move code em nm) other = 
  let em' = exactMatches code other
      mm' = matches      code other 
      nm' = mm' - em'
  in em == em' && nm == nm'
  

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1         = map (:[]) colors
allCodes n | n > 1 = concatMap (\x -> map (x:) (allCodes (n - 1))) colors
allCodes _         = []

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = solve' . allCodes $ length secret
  where solve' [] = []
        solve' (x:xs) =
          let move = getMove secret x
              rest = filterCodes move xs
          in move:(solve' rest)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
