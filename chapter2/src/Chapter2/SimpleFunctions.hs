module Chapter2.SimpleFunctions where

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
maxmin list = let h = head list
    in if null (tail list)
        then (h, h)
        else (if h > t_max then h else t_max
            , if h < t_min then h else t_min)
            where t = maxmin (tail list)
                  t_max = fst t
                  t_min = snd t

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

data TimeMachine = TimeMachine Manufacturer Int String TemporalDirection Float
                 deriving Show

clientName :: Client -> String
clientName (GovOrg name)                         = name
clientName (Company name _ _ _)                  = name
clientName (Individual (Person fName lName _) _) = fName ++ " " ++ lName

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

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
discount (TimeMachine manufacturer model name direction cost) sale = TimeMachine manufacturer model name direction (cost*sale)

fireSale :: [TimeMachine] -> Float -> [TimeMachine]
fireSale [] _        = []
fireSale (x:xs) sale = discount x sale : (fireSale xs sale)

sorted :: [Integer] -> Bool
sorted []             = True
sorted [_]            = True
sorted (x1:xs@(x2:_)) = x1 <= x2 && sorted (xs)
