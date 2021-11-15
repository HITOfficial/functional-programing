myFunc x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y

add2C :: Num a => a -> a -> a
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (x, y, z) = x + y + z

add3C :: Num a => a -> a -> a -> a
add3C x y z = x + y + z

fiveToPower_ :: Integer -> Integer
fiveToPower_ a = 5 ^ a

_toPower5 :: Integer -> Integer
_toPower5 a = a ^ 5

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 a = 5 - a

subtr5From_ :: Num a => a -> a
subtr5From_ a = a - 5

isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s
