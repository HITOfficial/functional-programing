sum2' :: Num a => [a] -> a
sum2' xs = loop 0 xs
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (x + acc) xs

sum3' :: Num a => [a] -> a
sum3' = loop 0
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (acc * x) xs

length'2 :: [a] -> Int
length'2 = loop 0
  where
    loop acc [] = 0
    loop acc (_ : xs) = loop (acc + 1) xs

isOdd :: (Ord a, Num a) => a -> Bool
isOdd n
  | n <= 0 = False
  | otherwise = isEven (n - 1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n
  | n <= 0 = False
  | otherwise = isOdd (n - 1)

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x : xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
  where
    leftPart xs = [y | y <- xs, y <= x]
    rightPart xs = [y | y <- xs, y > x]

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _ = False

fst2Divider :: Integral a => [a] -> Bool
fst2Divider (x : y : _) | x `mod` y == 0 = True
fst2Divider _ = False