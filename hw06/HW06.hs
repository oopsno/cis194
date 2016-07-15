{-# OPTIONS_GHC -Wall #-}
module HW06 ( main, fastFib ) where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:scanl (+) 1 fibs2

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons curr rest) = curr : streamToList rest

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap fn (Cons curr rest) = Cons (fn curr) (fmap fn rest)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = let fx = f x in Cons x (sIterate f fx)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons curr rest) second = Cons curr (sInterleave second rest)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons curr rest) = curr : sTake (n - 1) rest

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = fmap (ru . (+1)) nats
  where ru n = if even n then 1 + ru (div n 2) else 0

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate prand
  where prand r = (1103515245 * r + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ~110 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ~0.04 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = kernel x x xs
  where kernel min max [] = Just (min, max)
        kernel min max (x:xs) =
          let min' = if min > x then x else min
              max' = if max < x then x else max
          in  min' `seq` max' `seq` kernel min' max' xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

type FibMat = (Integer, Integer, Integer)
fastFib :: Int -> Integer
fastFib = pick . ($ unit) . go
  where sqr :: FibMat -> FibMat
        sqr (a, b, c) = let aa = a * a
                            bb = b * b
                            cc = c * c
                            ab = a * b
                            bc = b * c
                        in aa `seq` bb `seq` cc `seq` ab `seq` bc `seq` (aa + bb, ab + bc, bb + cc)
        mul (a, b, c) = let apb = a + b in apb `seq` (apb, a, b)
        unit = (1, 1, 0)
        go 0 = id
        go 1 = id
        go n = case n `rem` 2 of
                 0 -> let n' = div n 2 in sqr . go n'
                 1 -> let n' = (n - 1) `div` 2 in mul . sqr . go n'
        pick (x, _, _) = x
