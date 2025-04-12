module ZeroOne where 

-- Sum an integer from 0 to n 
sumtorial :: Integer -> Integer 
sumtorial 0 = 0 
sumtorial n = n + sumtorial (n-1) 

hailstone :: Integer -> Integer 
hailstone n 
  | n `mod` 2 == 0 = n `div` 2 
  | otherwise = 3*n + 1 

foo :: Integer -> Integer 
foo 0 = 16 
foo 1 
  | "Haskell" > "C++" = 3 
  | otherwise         = 4 
foo n 
  | n < 0             = 0 
  | n `mod` 17 == 2   = -43 
  | otherwise         = n + 3 

isEven  :: Integer -> Bool 
isEven n 
  | n `mod` 2 == 0 = True 
  | otherwise      = False 

hailstoneSeq :: Integer -> [Integer] 
hailstoneSeq 1 = [1] 
hailstoneSeq n = n : hailstoneSeq (hailstone n) 

intListLength :: [Integer] -> Integer 
intListLength [] = 0 
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x:[])     = []
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

toDigits :: Integer -> [Integer]
toDigits x
  | x < 0 = []
  | x < 10 = [x]
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

myDoubleEveryOther :: [Integer] -> [Integer]
myDoubleEveryOther x = map (\(a,b) -> a*b) $ zip x $ cycle [2,1]

-- Doubles every other number from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

--We can also define this as concat . map, but this works for any foldable t instead of [a].
myConcatMap :: Foldable t => (a -> [b]) -> t a -> [b]
myConcatMap f = foldr (\a bs -> f a ++ bs) [] 

-- Sums all digits of a number, treating numbers >9 as separate digits
sumDigits :: [Integer] -> Integer
sumDigits = sum . myConcatMap toDigits

myValidate :: Integer -> Bool
myValidate x = mod (sumDigits . myDoubleEveryOther $ toDigits x) 10 == 0

-- Validates the number using the Luhn algorithm
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []  -- Base case: no disks to move
hanoi n source auxiliary target =
    hanoi (n-1) source target auxiliary ++  -- Move n-1 disks from source to auxiliary
    [(source, target)] ++                   -- Move the nth disk from source to target
    hanoi (n-1) auxiliary source target     -- Move the n-1 disks from auxiliary to target
