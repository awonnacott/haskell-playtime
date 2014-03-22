module Bate (
	ptshiftct,
	ctshiftpt,
	ptvigct,
	ctvigpt,
	friedman,
	bcproc,
	enigma,
	linDio
	) where

import Prelude hiding (gcd)
import Data.Char (chr, ord)
import Data.List (transpose)
import List (count, countOrd, gcd)

i2c :: Int -> Char
i2c = chr . (97 +)
c2i :: Char -> Int
c2i = (- 97 +) . ord
ccaddc :: Char -> Char -> Char
ccaddc c1 c2 = i2c $ (c2i c1) + (c2i c2)
ccaddmc :: Char -> Char -> Char
ccaddmc c1 c2 = i2c $ mod ((c2i c1) + (c2i c2)) 26
ccsubc :: Char -> Char -> Char
ccsubc c1 c2= i2c $ (c2i c1) - (c2i c2)
ccsubmc :: Char -> Char -> Char
ccsubmc c1 c2 = i2c $ mod ((c2i c1) - (c2i c2)) 26

kp2c :: Int -> Char -> Char
kp2c k = i2c . (k +) . (c2i)

ptshiftct :: Int -> String -> String
ptshiftct k = map (i2c . (+ k) . c2i)
ctshiftpt :: Int -> String -> String
ctshiftpt k = ptshiftct (-k)

ptvigct :: [Int] -> String -> String
ptvigct kl = zipWith kp2c (cycle kl)
ctvigpt :: [Int] -> String -> String
ctvigpt kl = ptvigct (map (0-) kl)


tryfriedman dl ct = map (\d -> count $ zipWith (==) ct (drop d ct)) dl

friedman ct = (zip [1..]) . (map snd . head . tail) $ (tryfriedman [1..6] ct)

everyn _ [] = []
everyn n l = take n l : (everyn n (drop n l))

frequency :: (Integral a) => String -> [(Char, a)]
frequency list = map (\(x,y) -> (x, y-1)) (countOrd (list ++ (map i2c [(-32)..(-7)])))

breakapart lk ct = transpose (everyn lk ct)
breakcount lk ct = (map frequency) (breakapart lk ct)

bcproc lk ct = (map $ map snd) $ breakcount lk ct

klookupc :: (Eq a) => a -> [(a, Char)] -> Char
klookupc _ [] = ' '
klookupc key ((k,v):kl) = if k == key then v else (klookupc key kl)
mkcycle :: Char -> [(Char, Char)] -> [Char]
mkcycle lastk [] = lastk:[]
mkcycle ' ' ((a,b):xs) = a:(mkcycle b xs)
mkcycle lastk kl = lastk:(mkcycle (klookupc lastk kl) (filter (\(x,y) -> x /= lastk) kl))
splitAtFirst :: (Eq a) => a -> [a] -> ([a], [a])
splitAtFirst _ [] = ([], [])
{-splitAtFirst k (x:y:ys) = 
goodsplit :: (Eq a) => [a] -> [[a]]
goodsplit [] = []
goodsplit (x:xs) = (x:before):after
    where (before, after) = splitAtFirst x xs -}
enigma :: [[Char]] -> [[Char]]
enigma = (map {--$ goodsplit .--} (mkcycle ' ')) . (\(a:b:c:d:e:f:xs) -> [zip a d, zip b e, zip c f]) . (take 6) . (transpose)