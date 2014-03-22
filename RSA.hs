module RSA
( linDio
, encode
, decode
, convert
, decode903
) where

import Prelude hiding (gcd)
import Data.Char (chr, ord)
import List (gcd, mergesort)

-- RSA version 1.5

ee :: Integral a => (a,a,a,a) -> [(a,a,a,a)]
ee (m, k, n, d) = if d == 0 then [] else (m,k,n,d) : ee (n, quot n d, d, mod n d)
rewrite :: Integral a => (a,a,a,a) -> (a,[(a,a)])
rewrite (m,k,n,d) = (d,[(m,1),(n,-k)])
sub1 :: Integral a => (a, [(a, a)]) -> (a, [(a, a)]) -> (a, [(a, a)])
sub1 (c, exprs) (d, repls) = (c, foldl1 (++) [if x == d then [(r, s*y) | (r,s) <- repls] else [(x,y)] | (x,y) <- exprs])
subl :: Integral a => (a, [(a, a)]) -> [(a, [(a, a)])] -> (a, [(a, a)])
subl (c, exprs) subs = foldl sub1 (c,exprs) subs
collect :: Integral a => [(a,b)] -> [(a,b)] -- Collects adjacent terms where the coefficient comes as the second item in the pair
collect ((x,x2):(y,y2):xs) = let ((z,z2):zs) = collect ((y,y2):xs) in if x == z then (x,x2+z2):zs else (x,x2):(z,z2):zs
collect ((x,x2):xs) = [(x,x2)]
collect [] = []

linDio :: Integral a => a -> a -> a -> (a,a)
linDio _ _ 0 = (0,0)
linDio 1 _ c = (c,0)
linDio _ 1 c = (0,c)
linDio a b 1 = 
    let subs = map rewrite (ee (a, quot a b, b, mod a b))
        subd = snd $ subl (last subs) (reverse $ init subs)
    in (\((x,x2):(y,y2):xs) -> (x2,y2)) . (if a > b then reverse else (\a -> a)) . collect . mergesort $ subd
linDio a b c = let g = gcd a b in if (mod c g) /= 0 then error "No solutions"
    else if g /= 1 then linDio (quot a g) (quot b g) (quot c g)
    else let q = quot c g in (\(x,y) -> (x*q,y*q)) $ linDio a b (quot c q)

i2c :: Int -> Char
i2c = chr . (86 +)
c2i :: Char -> Int
c2i = (- 86 +) . ord
ps :: [a] -> [[a]]
ps (x:y:xs) = [x,y]:(ps xs)
ps x = [x,x]
convert :: Show a => [a] -> String
convert x = map i2c $ map read $ init $ init (ps $ show x)

{-
p :: Num a => a
p = 577
q :: Num a => a
q = 673
m :: Num a => a
m = p * q
k :: Num a => a
k = 154829
u :: Num a => a
u = 5 -- fst $ solvLinDio k ((p-1)*(q-1)) 1 -- (p-1)*(q-1) = phi m
-}

encode :: Integral a => a -> a -> a -> [a] -> [a]
encode p q k = map (\x -> mod (x ^ k) (p*q))
decode :: Integral a => a -> a -> a -> [a] -> [a]
decode p q k = map (\x -> mod (x ^ (fst $ linDio k ((p-1)*(q-1)) 1)) (p*q))

--decode903 :: (Integral a, Show a) => a -> a -> a -> [a] -> [String]
decode903 p q k = (map convert) . (decode p q k)