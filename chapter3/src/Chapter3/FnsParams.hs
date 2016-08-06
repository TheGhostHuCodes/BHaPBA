module Chapter3.FnsParams where

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f(x + 2)

equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x, y) -> x == y) t

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                               "Alejandro" -> "Hello, writer"
                               _           -> "Welcome, " ++ name
                     ) names
