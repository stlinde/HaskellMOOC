-- | 

module Documents.Repos.Haskell.HaskellMOOC.LectureFour where

-- Pattern matching on tuples
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Sum all numbers that are paired with true
sumIf :: [(Bool, Int)] -> Int
sumIf [] = 0
sumIf ((True, x):xs) = x + sumIf xs
sumIf ((False, _):xs) = sumIf xs

-- Folding
-- Consider the following functions
sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x + sumNumbers xs

myMaximum :: [Int] -> Int
myMaximum []     = 0
mymaximum (x:xs) = go x xs
  where
    go biggest []     = biggest
    go biggest (x:xs) = go (max biggest x) xs

countNothings :: [Maybe a] -> Int
countNothings [] = 0
countNothings (Nothing : xs) = 1 + countNothings xs
countNothings (Just _  : xs) = countNothings xs

-- They all take a list and produce a single value.
-- We could use foldr from the Prelude module instead.
sumNumbersFold :: [Int] -> Int
sumNumbersFold xs = foldr (+) 0 xs

-- Defining the map function using foldr
myMap g xs = foldr helper [] xs
  where
    helper y ys = g y : ys
