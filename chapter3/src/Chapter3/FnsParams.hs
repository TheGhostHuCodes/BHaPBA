module Chapter3.FnsParams where

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f(x + 2)
