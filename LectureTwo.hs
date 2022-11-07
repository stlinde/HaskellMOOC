-- | Lecture II - Either You Die a Hero

module Documents.Repos.Haskell.HaskellMOOC.LectureTwo where

-- Repeating string
repeatString :: Integer -> String -> String
repeatString n str = repeatHelper n str ""

repeatHelper :: Integer -> String -> String -> String
repeatHelper n str result
  | n == 0    = result
  | otherwise = repeatHelper (n-1) str (result ++ str)

-- fibonacci fast
fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a + b) (n - 1)
-- Side note: apostrophe is often used at the end of a function name
-- if defining a helper function.

-- Guards can be used when you have multiple different return
-- expressions depending on some condition
describe :: Int -> String
describe n
  | n == 2        = "Two"
  | even n        = "Even"
  | n == 3        = "Three"
  | n > 100       = "Big!!!"
  | otherwise     = "The number " ++ show n

-- Guards and pattern matching can even be used together
guessAge :: String -> Int -> String
guessAge "Griselda" age
  | age < 47 = "Too low!"
  | age > 47 = "Too high!"
  | otherwise = "Correct!"
guessAge "Hansel" age
  | age < 12 = "Too low!"
  | age > 12 = "Too high!"
  | otherwise = "Correct!"

-- Lists are used to store multiple values of the same type.
-- In other words Haskell lists are homogenous.

-- Some list operations come from the module Data.List


dropThreeFour :: [a] -> [a]
dropThreeFour xs = take 2 xs ++ drop 4 xs

-- The Maybe type.
loginMaybe :: String -> Maybe String
loginMaybe "f4bolous!" = Just "unicorn73"
loginMaybe "swordfish" = Just "megahacker"
loginMaybe _           = Nothing

-- Maybe for pattern matching
perhapsMultiply :: Int -> Maybe Int -> Int
perhapsMultiply i Nothing = i
perhapsMultiply i (Just j) = i * j

intOrZero :: Maybe Int -> Int
intOrZero Nothing = 0
intOrZero (Just i) = i

safeHead :: [a] -> Maybe a
safeHead xs = if null xs then Nothing else Just (head xs)

headOrZero :: [Int] -> Int
headOrZero xs = intOrZero (safeHead xs)

-- Using Either to construct list with different types


-- Case of expression
describeCase :: Integer -> String
describeCase n = case n of 0 -> "zero"
                           1 -> "One"
                           2 -> "An even prime"
                           n -> "number " ++ show n

-- parse country code into country name, returns Nothing
parseCountry :: String -> Maybe String
parseCountry "FI" = Just "Finland"
parseCountry "SE" = Just "Sweden"
parseCountry _ = Nothing

-- case-of expression can be replaced with a helper function
sentenceType :: String -> String
sentenceType sentence = case last sentence of '.' -> "statement"
                                              '?' -> "question"
                                              '!' -> "exclamation"
                                              _   -> "not a sentence"

sentenceTypeHelper :: String -> String
sentenceTypeHelper sentence = classify (last sentence)
  where
    classify '.' = "statement"
    classify '?' = "question"
    classify '!' = "exclamation"
    classify _   = "not a sentence"


