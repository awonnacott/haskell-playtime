module NumberTheory
( divisors
, factors
, prime
, pfactors
, primes
, upf
, unit
, phi
, order
, legendre
) where

import Prelude hiding (gcd)
import Data.List (genericLength)
import List (gcd, countOrd)

divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1 .. n], 0 == n `mod` x]
factors :: Integral a => a -> [a]
factors = init . tail . divisors
prime :: Integral a => a -> Bool
prime = ([] ==) . factors
pfactors :: Integral a => a -> [a]
pfactors n = [x | x <- (divisors n), prime x]
primes :: Integral a => [a]
primes = [x | x <- [1..], prime x]
upf :: Integral a => a -> [a]
upf n = if prime n then [n] else first : upf (quot n first)
    where first = (head $ factors n)

--U[n], or the elements in Z[n] relatively prime to n
unit :: Integral a => a -> [a]
unit n = [x | x <- [1..n], 1 == gcd x n]
--the length of U[n]
phi' :: Integral a => a -> a
phi' = genericLength . unit
phi :: Integral a => a -> a
phi = sum . (map (\(x,y) -> x^y-x^(y-1))) . countOrd . upf
order :: Integral a => a -> a -> a
order m s = order' s s m where order' s x m = if x == 1 then 1 else (+) 1 $ order' s (mod (s*x) m) m
psi :: Integral a => a -> a -> a
psi p d = genericLength [d | x <- (map (order p) (unit p)), x == d]
ef :: Integral a => a -> a -> a
ef p n = genericLength [1 | x <- [1..p], (==) 1 $ mod (x^n) p]
eff :: Integral a => a -> a
eff = sum . (map phi) . divisors

legendre' :: Integral a => a -> a -> Bool
legendre' a p = elem (mod a p) $ map (\x -> mod (x*x) p) [1..(p-1)]
legendre :: Integral a => a -> a -> a
legendre t p = let a = mod t p in
    if a == 0 then 0
    else if a == 1 then 1
    else if a == (p-1) then (if or [p == 2, (mod p 4) == 1] then 1 else -1)
    else if a == 2 then (if elem (mod p 8) [1,7] then 1 else -1)
    else if prime a then (if (mod p 4) * (mod a 4) /= 9 then 1 else -1) * (legendre p a)
    else product $ map (\x -> legendre x p) (upf a)