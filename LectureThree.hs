-- | Lecture III - Catamorphic

module Documents.Repos.Haskell.HaskellMOOC.LectureThree where

import Data.List

-- Higher order functions
-- Perentheses are needed as the type Int -> Int -> Int would be the
-- Type for a function taking two Int arguments
applyTo1 :: (Int -> Int) -> Int
applyTo1 f = f 1

addThree :: Int -> Int
addThree x = x + 3

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

-- Filter and map
positive :: Int -> Bool
positive x = x > 0

onlyPositive :: [Int] -> [Int]
onlyPositive xs = filter positive xs

mapBooleans :: (Bool -> b) -> [b]
mapBooleans f = map f [False, True]

-- Constructors as arguments
wrapJust :: [a] -> [Maybe a]
wrapJust xs = map Just xs

-- Palindrome
palindrome :: String -> Bool
palindrome str = str == reverse str

palindromes :: Int -> [String]
palindromes n = filter palindrome (map show [1..n])

-- Partial functions
add :: Int -> Int -> Int
add a b = a + b

addThreePartial :: Int -> Int
addThreePartial = add 3

between :: Integer -> Integer -> Integer -> Bool
between lo high x = x < high && x > lo

-- Lambda
findTrades :: Int -> Int -> [Int] -> [Int]
findTrades lower upper xs =
  filter (\x -> x < upper && x > lower) xs

tradesToLimit :: Int -> [Int] -> [Int]
tradesToLimit limit trades = take viable trades
  where
    viable = length . takeWhile (< limit) $ scanl1 (+) trades

-- whatFollows
-- The naive implementation
substringsOfLength :: Int -> String -> [String]
substringsOfLength n string = map shorten (tails string)
  where shorten s = take n s

whatFollowsNaive :: Char -> Int -> String -> [String]
whatFollowsNaive c k string =
  map tail (filter match (substringsOfLength (k + 1) string ))
    where match sub = take 1 sub == [c]


-- Using .
whatFollows c k =
  map tail
  . filter ((==[c]) . take 1)
  . map (take (k + 1))
  . tails


-- Find substring
findSubstring :: String -> String -> String
findSubstring chars = takeWhile (\x -> elem x chars)
                      . dropWhile (\x -> not $ elem x chars)

-- Using : to build lists using recursion
descend :: Integer -> [Integer]
descend 0 = []
descend n = n : descend (n-1)

-- Building a list by iterating a function n times
iter :: (Eq t1, Num t1) => (t2 -> t2) -> t1 -> t2 -> [t2]
iter f 0 x = [x]
iter f n x = x : iter f (n - 1) (f x)

-- Splitting a string into pieces a at a given character
split :: Char -> String -> [String]
split c [] = []
split c xs = start : split c (drop 1 rest)
  where start = takeWhile (/=c) xs
        rest = dropWhile (/=c) xs

-- Pattern matching for lists
myhead :: [Int] -> Int
myhead [] = -1
myhead (first:rest) = first

mytail :: [Int] -> [Int]
mytail [] = []
mytail (first:rest) = rest

sumFirstTwo :: [Integer] -> Integer
sumFirstTwo (a:b:_) = a + b
sumFirstTwo _ = 0

-- Consuming a list
sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x + sumNumbers xs


myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = go x xs
  where go biggest [] = biggest
        go biggest (x:xs) = go (max biggest x) xs

-- tail recursion sumNumbers
sumNumbersTail :: [Int] -> Int
sumNumbersTail xs = go 0 xs
  where
    go sum [] = sum
    go sum (x:xs) = go (sum + x) xs

-- Recursion where the non-tail recursive function is
-- faster than the tail recursive one.
-- This is due to the fact that the : operator works in
-- constant time while the ++ operator needs linear time.
doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = 2 * x : doubleList xs

doubleListTail :: [Int] -> [Int]
doubleListTail xs = go [] xs
  where
    go result [] = result
    go result (x:xs) = go (result ++ [2*x]) xs


-- List comprehension
firstLetters string = [ char | (char:_) <- words string]
