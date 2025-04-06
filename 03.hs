module ZeroThree where

import Data.List (group, sort)

-- Skip function. Returns a list containing the input and every nth value from the input
-- i.e. s [1, 2, 3] = [[1, 2, 3], [2], [3]]
s :: [a] -> [[a]]
s [] = []
s l = [o n l | n <- [1..length l]] 

-- Skip N function. Helper function for Skip which performs a list comprehension to grab each nth item from a list.
o :: Int -> [a] -> [a]
o n l = [x | (x, i) <- zip l [1..], (mod i n) == 0]

-- Local Maxima function takes a list of Integers and outputs a list of local maxima.
-- i.e. every value which is greater than its neighbors.
-- This version is code golfed
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima xs = [y | (x, y, z) <- zip3 xs (tail xs) (tail (tail xs)), y > x && y > z]

-- Local Maxima Recursive does the same as above, but instead of list comprehension we use recursion.
localMaximaRecursive :: [Integer] -> [Integer]
localMaximaRecursive (x:y:z:zs) 
   | y > x && y > z = y : localMaximaRecursive (y:z:zs)
   | otherwise = localMaximaRecursive (y:z:zs)
localMaximaRecursive _ = []

-- Generates a visual histogram from a list of numbers between 0-9.
histogram :: [Integer] -> String
histogram xs = unlines $ map line [maxCount, maxCount-1..1] ++ [footer]
   where 
      occurrences = count xs
      maxCount    = maximum occurrences
      line level  = [if count >= level then '*' else ' ' | count <- occurrences]
      footer      = "==========\n0123456789\n"
-- Helper function for histogram
count :: [Integer] -> [Int]
count xs = [length [x | x <-xs, x == n] | n <- [0..9]]
