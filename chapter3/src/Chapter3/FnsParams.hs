module Chapter3.FnsParams where

import Chapter3.ParamPoly

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f(x + 2)

equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x, y) -> x == y) t

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                               "Alejandro" -> "Hello, writer"
                               _           -> "Welcome, " ++ name
                     ) names

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n*x

filterOnes :: [Integer] -> [Integer]
filterOnes l = filter (\i -> i == 1) l

filterANumber :: Integer -> [Integer] -> [Integer]
filterANumber num l = filter (\i -> i == num) l

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f l = filter (not . f) l

isGovOrg :: Client a -> Bool
isGovOrg (GovOrg _ _) = True
isGovOrg _ = False

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs l = filter isGovOrg l

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f = \(x, y) -> f x y

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = \x y -> f (x, y)

(***) :: (a -> b) -> (c -> d) -> ((a, c) -> (b, d))
f *** g = \(x, y) -> (f x, g y)

myDuplicate :: a -> (a, a)
myDuplicate x = (x, x)

formula1 :: Integer -> Integer
formula1 = myUncurry (+) . (((*7) . (+2)) *** (*3)) . myDuplicate
