import Data.Char

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

swap :: (Int, Char) -> (Char, Int)
swap (a, b) = (chr a, ord b)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (a, b, c) = a == b && b == c

sgn :: Int -> Int
sgn n =
  if n < 0
    then -1
    else
      if n == 0
        then 0
        else 1

absInt :: Int -> Int
absInt n =
  if n < 0
    then (-1) * n
    else n

min2Int :: (Int, Int) -> Int
min2Int (a, b) =
  if a > b
    then b
    else a

absInt2 :: Int -> Int
absInt2 n
  | n > 0 = n
  | otherwise = -n

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _ = False

or' :: (Bool, Bool) -> Bool
or' (a, b) = a || b

and' :: (Bool, Bool) -> Bool
and' (a, b) = a && b

nand' :: (Bool, Bool) -> Bool
nand' (a, b) = not (a && b)

xor' :: (Bool, Bool) -> Bool
xor' (a, b)
  | a && b = True
  | not a && not b = False
  | otherwise = True

not' :: Bool -> Bool
not' b = case b of
  True -> False
  False -> True

absInt3 n =
  case (n >= 0) of
    True -> n
    _ -> -n

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
  let d = sqrt (b * b - 4 * a * c)
      e = 2 * a
   in (((-1) * b - d) / e, ((-1) * b + d) / e)

roots2 :: (Double, Double, Double) -> (Double, Double)
roots2 (a, b, c) = (((-1) * b - d) / e, ((-1) * b + d) / e)
  where
    d = sqrt (b * b - 4 * a * c)
    e = 2 * a
