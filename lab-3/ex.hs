f1 = \(x) -> x - 2

f2 = \(x, y) -> sqrt ((x ^ 2) + (y ^ 2))

f3 = \(x, y, z) -> sqrt ((x ^ 2) + (y ^ 2) + (z ^ 2))

f4 = \x -> (*) x

f5 = \x -> (^) x

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x : xs) = (x ^ 2) + sum' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith func [] = 0
sumWith func (x : xs) = (func x) + (sumWith func xs)

sum'' (x : xs) = sumWith (\x -> x) xs

sumSqr'' (x : xs) = sumWith (\x -> sqrt x) xs

sumCube'' (x : xs) = sumWith (\x -> x ^ 2) xs

sumAbs'' (x : xs) = sumWith (\x -> abs x) xs

listLength'' (x : xs) = sumWith (\x -> 1) xs

onlyEven [] = []
onlyEven (x : xs)
  | x `mod` 2 == 0 = x : onlyEven xs
  | otherwise = onlyEven xs

doubleElems [] = []
doubleElems (x : xs) = 2 * x : doubleElems xs

map' :: (a -> b) -> [a] -> [b]
map' func [] = []
map' func (x : xs) = (func x) : map' func xs

doubleElems' [] = []
doubleElems' (x : xs) = map' (\x -> 2 * x) xs

sqrElems' [] = []
sqrElems' (x : xs) = map' (\x -> sqrt x) xs

sumWith' func [] = 0
sumWith' func (x : xs) = func x + sumWith func xs

prodWith' func [] = 1
prodWith' func (x : xs) = func x + sumWith func xs

sumWith'' :: Num a => (a -> a) -> [a] -> a
sumWith'' = go 0
  where
    go acc g [] = 0
    go acc g (x : xs) = go (g x + acc) g xs

prodWith'' :: Num a => (a -> a) -> [a] -> a
prodWith'' = go 1
  where
    go acc g [] = acc
    go acc g (x : xs) = go (g x * acc) g xs
