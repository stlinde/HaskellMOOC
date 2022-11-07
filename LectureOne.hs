-- | Lecture One - ... And So it Begins

module Documents.Repos.Haskell.HaskellMOOC.LectureOne where

-- the golden ratio
phi :: Double
phi = (sqrt 5 + 1) / 2

polynomial :: Double -> Double
polynomial x = x^2 - x - 1

f x = polynomial $ polynomial x

listOfPolynomials :: [Double] -> [Double]
listOfPolynomials xs = map f xs

-- if statements
checkPassword :: [Char] -> [Char]
checkPassword password = if password == "swordfish"
                         then "You're in"
                         else "ACCESS DENIED"

absoluteValue :: Double -> Double
absoluteValue n = if n < 0 then -n else n

login :: [Char] -> [Char] -> [Char]
login user password = if user == "unicorn73"
                      then if password == "f4bulous!"
                           then "unicorn73 logged in"
                           else "wrong password"
                      else "unknown user"

-- Local definitions (let and where)
circleArea :: Double -> Double
circleArea r = pi * rsquare
  where pi = 3.1415
        rsquare = r * r

circleAreaLet :: Double -> Double
circleAreaLet r = let pi = 3.1415926
                      rsquare = r * r
                  in pi * rsquare

-- Can also include functions
circleAreaWhere r = pi * square r
  where pi = 3.1415926
        square x = x * x

circleAreaLetFunc r = let pi = 3.1415926
                          square x = x * x
                      in pi * square r


-- Pattern matching
greet :: String -> String -> String
greet "Finland" name = "Hei, " ++ name
greet "Italy" name = "Ciao, " ++ name
greet "England" name = " How do you do, " ++ name
greet _ name = "Hello, " ++ name

describe :: Integer -> String
describe 0 = "zero"
describe 1 = "one"
describe 2 = "an even prime"
describe n = "The number " ++ show n

loginPattern :: String -> String -> String
loginPattern "unicorn73" "f4bulous!" = "unicorn73 logged in"
loginPattern "unicorn73" _           = "wrong password"
loginPattern _           _           = "unknown user"

-- Recursion
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)
