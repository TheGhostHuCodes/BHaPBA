{-# LANGUAGE ViewPatterns #-}
module Chapter2.SimpleFunctions where

import Data.Char (toUpper)

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
[]     +++ list2 = list2
(x:xs) +++ list2 = x:(xs +++ list2)

reverse2 :: [a] -> [a]
reverse2 list = if null list
    then []
    else reverse2 (tail list) +++ [head list]

maxmin :: Ord a => [a] -> (a, a)
maxmin [x] = (x, x)
maxmin (x:xs) = (if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min)
                where (xs_max, xs_min) = maxmin xs

data Client = GovOrg        String
            | Company       String Integer Person String
            | Individual    Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data TemporalDirection = Past | Future | Both
                       deriving Show

data Manufacturer = Manufacturer String
                  deriving Show

data TimeMachine = TimeMachine { manufacturer :: Manufacturer
                                , model :: Int
                                , modelName :: String
                                , direction :: TemporalDirection
                                , cost ::Float }
                   deriving Show

clientName :: Client -> String
clientName (GovOrg name)                         = name
clientName (Company name _ _ _)                  = name
clientName (Individual (Person fName lName _) _) = fName ++ " " ++ lName

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n < 0     = Nothing
ifibonacci 0             = Just 0
ifibonacci 1             = Just 1
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))
                         in Just (f1 + f2)

genderTuple :: Client -> (Int, Int)
genderTuple (Individual (Person _ _ Male) _) = (1, 0)
genderTuple (Individual (Person _ _ Female) _) = (0, 1)
genderTuple _ = (0, 0)

genderCount :: [Client] -> (Int, Int)
genderCount []     = (0, 0)
genderCount (x:xs) = let i = genderTuple x
                   in (fst i + xs_men, snd i + xs_women)
                      where (xs_men, xs_women) = genderCount xs

discount :: TimeMachine -> Float -> TimeMachine
discount tm@(TimeMachine { cost = price }) sale =
    let newPrice = price*sale
    in tm { cost = newPrice }

fireSale :: [TimeMachine] -> Float -> [TimeMachine]
fireSale [] _        = []
fireSale (x:xs) sale = discount x sale : (fireSale xs sale)

sorted :: [Integer] -> Bool
sorted []             = True
sorted [_]            = True
sorted (x1:xs@(x2:_)) = x1 <= x2 && sorted (xs)

binom :: Int -> Int -> Int
binom 0 _ = 0
binom _ 0 = 1
binom x y | x == y = 1
binom n k = (binom (n-1) (k-1)) + (binom (n-1) k)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
specialMultiples n
    | multipleOf n 2 = show n ++ " is a multiple of 2"
    | multipleOf n 3 = show n ++ " is a multiple of 3"
    | multipleOf n 5 = show n ++ " is a multiple of 5"
    | otherwise      = show n ++ " is a beautiful number"

ackermann :: Integer -> Integer -> Integer
ackermann m n
    | m == 0          = n + 1
    | m > 0 && n == 0 = ackermann (m-1) 1
    | m > 0 && n > 0  = ackermann (m-1) (ackermann m (n-1))

unzipper :: [(a, a)] -> ([a], [a])
unzipper []     = ([], [])
unzipper (x:xs) = let (f, s) = x
                in (f:fs, s:ss)
                    where (fs, ss) = unzipper xs

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _                               = False

data ClientR = GovOrgR     { clientRName :: String }
             | CompanyR    { clientRName :: String
                            , companyId :: Integer
                            , person :: PersonR
                            , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                        , lastName :: String }
                        deriving Show

greet :: ClientR -> String
greet IndividualR { person = PersonR { firstName = fn} } = "Hi, " ++ fn
greet CompanyR    { clientRName = c }                    = "Hello, " ++ c
greet GovOrgR     { }                                    = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
    let newName = (toUpper initial):rest
    in  p { firstName = newName }
nameInCapitals p@(PersonR { firstName = ""}) = p
