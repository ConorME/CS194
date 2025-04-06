module ZeroFour where

-- Here are two functions.
-- We want to re-write them as idiomatic Haskell
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
   | even x = (x - 2) * fun1 xs
   | otherwise = fun1 xs

-- Version 1 using list comprehensions and folds.
f1 :: [Integer] -> Integer
f1 xs = foldl (*) 1 [x - 2 | x <- xs, even x]

-- Version 2 using some simple functions and function composition.
f1_2 :: [Integer] -> Integer
f1_2 xs = go xs  
   where go = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
   | even n    = n + fun2 (n `div` 2)
   | otherwise = fun2 (3 * n + 1)

f2 :: Integer -> Integer
f2 n = sum . filter even . takeWhile (>1)  $ iterate collatz n
   where collatz x | even x    = x `div` 2
                   | otherwise = 3 * x + 1

-- A tree data type.
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

-- This height function takes a tree and returns 0 if it's a leaf or the height of the node.
height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

-- Quick little balanced-esque insert function for our tree type.
-- We are simply return a Node of height zero with two leaves if the insert point is a leaf.
-- Otherwise we pick the shortest child to insert into to keep ourselves relatively balanced.
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h left val right)
    | height left <= height right = let newLeft = insert x left
                                    in Node (max (height newLeft) (height right) + 1) newLeft val right
    | otherwise                   = let newRight = insert x right
                                    in Node (max (height newRight) (height left) + 1) left val newRight

-- foldTree just acts as a pipeline to transform a list into a Tree. Simple enough.
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Funky xor function which operates on a list of Booleans.
xor :: [Bool] -> Bool
xor (x:xs) = foldl xor' x xs

-- xor binary operation to be based into the list xor function above.
xor' :: Bool -> Bool -> Bool
xor' a b = (a || b) && not (a && b)

-- Re-mapping of the map function to work off of foldr for some reason.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x init -> f x : init) []

-- 
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1) . (*2)) . (\n -> [x | x <- [1..n], notElem x [i + j + 2*i*j | i <- [1..n], j <- [1..n], 1 <= i, 1 <= j, i <= j]]) 
