{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1] 

-- Exercise 2 ----------------------------------------

instance (Eq a) => Eq (Poly a) where
    (==) (P lhs) (P rhs) = lhs == rhs
 
-- Exercise 3 -----------------------------------------

instance (Eq a, Num a, Show a) => Show (Poly a) where
    show (P xs) = intercalate " + " . reverse . filter (not . null) $ zipWith fmt xs [(0::Int)..]
      where fmt 0 _ = []
            fmt n 0 = show n
            fmt n i = 
               let xi = if i == 1 then "x" else "x^" ++ show i
                   xn = case n of
                          1    -> ""
                          (-1) -> "-"
                          _    -> show n
               in  xn ++ xi         

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P lhs) (P rhs) = if length lhs > length rhs
                         then plus' lhs (rhs ++ repeat 0)
                         else plus' (lhs ++ repeat 0) rhs
  where plus' xs ys = P $ zipWith (+) xs ys
              
  

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = sum . map P $ zipWith (\m n -> replicate n 0 ++ map (m*) ys) xs [0..]

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P (map negate xs)
    fromInteger i = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P xs) val = sum . zipWith (*) xs $ scanl (*) 1 (repeat val) 

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n d
      | n < 0     = error $ "nderiv: n(" ++ show n ++ ") < 0"
      | n == 0    = d
      | otherwise = nderiv (n - 1) (deriv d)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P xs) = P (zipWith (*) powers $ tail xs)
      where powers = scanl (+) 1 (repeat 1)

