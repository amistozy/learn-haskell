module Polynom where

import Data.Bifunctor (Bifunctor (bimap, first))

newtype Polynom = P [(Integer, Integer)] deriving (Eq)

makeItem :: Integer -> Integer -> Polynom
makeItem c d = P [(c, d)]

x :: Polynom
x = makeItem 1 1

deg :: Polynom -> Integer
deg (P xs)
  | null xs = -1
  | otherwise = snd $ head xs

coe :: Polynom -> Integer
coe (P xs)
  | null xs = 0
  | otherwise = fst $ head xs

instance Show Polynom where
  show (P xs)
    | null xs = "0"
    | otherwise = case concatMap format xs of
        ' ' : '+' : ' ' : s -> s
        ' ' : '-' : ' ' : s -> '-' : s
   where
    format (a, b)
      | b == 0 = sign ++ show a'
      | otherwise = sign ++ s1 ++ "x" ++ s2
     where
      a' = abs a
      sign = if a > 0 then " + " else " - "
      s1 = if a' == 1 then "" else show a'
      s2 = if b == 1 then "" else "^" ++ show b

add :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
add [] ys = ys
add xs [] = xs
add xs@(x : xs') ys@(y : ys')
  | b > d = x : add xs' ys
  | b < d = y : add xs ys'
  | e == 0 = add xs' ys'
  | otherwise = (a + c, b) : add xs' ys'
 where
  (a, b) = x
  (c, d) = y
  e = a + c

mult :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
mult xs = foldr (add . (\(a, b) -> map (bimap (* a) (+ b)) xs)) []

instance Num Polynom where
  P xs + P ys = P (add xs ys)
  P xs * P ys = P (mult xs ys)
  abs p = if signum p == -1 then negate p else p
  signum (P xs) = case xs of
    [] -> 0
    (a, _) : _ -> fromInteger $ signum a
  fromInteger n
    | n == 0 = P []
    | otherwise = P [(n, 0)]
  negate (P xs) = P $ map (first negate) xs

(//%) :: Polynom -> Polynom -> (Polynom, Polynom)
a //% b
  | p < 0 || q == 0 = (0, a)
  | otherwise = (c + q', r')
 where
  p = deg a - deg b
  (q, r) = coe a `quotRem` coe b
  c = makeItem q p
  a' = a - b * c
  (q', r') = a' //% b

(//), (%) :: Polynom -> Polynom -> Polynom
a // b = fst $ a //% b
a % b = snd $ a //% b