-- >> (then) (>>=) bind
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad

actSeq = putChar 'a' >> putChar 'g' >> putChar 'h' >> putChar '\n'

doActSeq = do
  putChar 'a'
  putChar 'g'
  putChar 'h'
  putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog =
  putStr "Your Happy number?"
    >> getLine
    >>= \n ->
      let num = read n :: Int
       in if num == 7
            then putStr "lucky 7!"
            else
              if odd num
                then putStrLn "odd number"
                else putStrLn "oh, even number"

twoQuestions :: IO ()
twoQuestions = do
  putStr "Your name"
  name <- getLine
  putStr "Your age"
  age <- getLine
  print (name, age)

newtype Box a = MkBox a deriving (Show)

-- instance Functor Box where
--   fmap f (MkBox x) = MkBox (f x)

data MyList a
  = EmptyList
  | Cons a (MyList a)
  deriving (Show)

instance Functor MyList where
  fmap _ EmptyList = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x

infixl 0 >$>

(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f

infixl 9 >.>

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : xs) = Just xs

extractMaybe :: Maybe a -> a
extractMaybe Nothing = error "Empty"
extractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe = Just

(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >^$> _ = Nothing
(Just x) >^$> f = f x

infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >.>> g = \x -> g (extractMaybe (f x))

doSafeTail3x :: [a] -> Maybe [a]
doSafeTail3x xs = do
  t1 <- safeTail xs
  t2 <- safeTail t1
  t3 <- safeTail t2
  return t3

safeTail3x :: [a] -> Maybe [a]
safeTail3x xs =
  safeTail xs >>= \t1 ->
    safeTail t1 >>= \t2 ->
      safeTail t2 >>= \t3 ->
        return t3

safeTail3x' :: [a] -> Maybe [a]
safeTail3x' xs = return xs >>= safeTail >>= safeTail >>= safeTail

f5 :: Int -> Int -> Int -> Int
f5 x y z = 1000 `div` x + 100 `div` y + 10 `div` z

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
  | y /= 0 = Just $ x `div` y
  | otherwise = Nothing

safeF5 :: Int -> Int -> Int -> Maybe Int
safeF5 x y z =
  case (safeDiv 1000 x) of
    Nothing -> Nothing
    Just (iOverX) ->
      case (safeDiv 100 y) of
        Nothing -> Nothing
        Just (iOverY) ->
          case (safeDiv 10 z) of
            Nothing -> Nothing
            Just (iOverZ) -> Just $ iOverX + iOverY + iOverZ

safeF5' :: Int -> Int -> Int -> Maybe Int
safeF5' x y z = do
  iOverX <- safeDiv 1000 x
  iOverY <- safeDiv 100 y
  iOverZ <- safeDiv 10 z
  return $ iOverX + iOverY + iOverZ

xs1 :: [(Int, Int, Int)]
xs1 = [(x, y, z) | let xs = [1, 2], x <- xs, y <- xs, z <- xs]

doXs1 :: [(Int, Int, Int)]
doXs1 = do
  let xs = [1, 2]
  x <- xs
  y <- xs
  z <- xs
  return (x, y, z)

xs2 :: [(Int, Int, Int)]
xs2 = [(x, y, z) | let xs = [1 .. 3], x <- xs, y <- xs, z <- xs, x > y && y > z]

doXs2 :: [(Int, Int, Int)]
doXs2 = do
  let xs = [1 .. 3]
  x <- xs
  y <- xs
  z <- xs
  guard $ x > y && y > z
  return (x, y, z)

doXs2' :: [(Int, Int, Int)]
doXs2' = do
  let xs = [1 .. 3]
  x <- xs
  y <- xs
  z <- xs
  if x > y && y > z
    then return (x, y, z)
    else []

c3 = getLine >>= \l1 -> return (l1 ++ l1) >>= \l2 -> print [l1, l2]

doC3 = do
  l1 <- getLine
  l2 <- return (l1 ++ l1)
  print ([l1, l2])