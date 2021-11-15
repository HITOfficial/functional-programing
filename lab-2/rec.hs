fib :: (Num a, Eq a) => a -> a
fib n =
  if n == 0 || n == 1
    then 1
    else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x : xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x : xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = False
and' (x : xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs) = a == x || elem' a xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x : xs) = 2 * x : doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x : xs) = x ^ 2 : squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x : xs) =
  if x `mod` 2 == 0
    then x : selectEven xs
    else selectEven xs