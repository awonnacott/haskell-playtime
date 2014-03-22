module List (
	gcd,
	count,
	countOrd,
	split,
	interlace,
	mergesort,
	insert,
	foldl2,
	bubblesort,
	listgen,
	forlist
	) where

import Prelude hiding (gcd)
import Data.List (genericLength)

gcd :: Integral a => a -> a -> a
gcd x y = let a = max (abs x) (abs y); b = min (abs x) (abs y); m = a `mod` b
    in if m == 0 then b else gcd b m

count :: (Eq a, Integral b) => [a] -> [(a, b)]
count (x:xs) = (x, 1 + (genericLength $ filter (x ==) xs)) : (count $ filter (x /=) xs)
count [] = []
countOrd :: (Ord a, Integral b) => [a] -> [(a, b)]
countOrd = mergesort . count

split :: [a] -> ([a], [a])
split (x:y:ys) = (x:ys1, y:ys2) where (ys1, ys2) = split ys
split (x:xs) = ([x], xs)
split [] = ([],[])

interlace :: Ord a => [a] -> [a] -> [a]
interlace (x:xs) (y:ys) = if x < y then x : (interlace xs (y:ys)) else y : (interlace (x:xs) ys)
interlace [] y = y
interlace x [] = x

mergesort :: Ord a => [a] -> [a]
mergesort (x:y:z:xs) = interlace (mergesort s1) (mergesort s2) where (s1, s2) = split (x:y:z:xs)
mergesort (x:y:xs) = interlace [x] [y]
mergesort (x:xs) = [x]
mergesort [] = []

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y then x:y:ys else y:(insert x ys)

insert' :: Ord a => a -> [a] -> [a]
insert' = (\x list -> if list == [] then [x] else let y = head list; ys = tail list in if x < y then x:y:ys else y:(insert x ys))

foldl2 :: (a -> [a] -> [a]) -> [a] -> [a]
foldl2 f (x:y:xs) = f x $ foldl2 f (y:xs)
foldl2 f (x:xs) = f x []
foldl2 _ [] = []
{-
foldl2' :: Eq a => (a -> [a] -> [a]) -> [a] -> [a]
foldl2' = (\f list -> (\this f list -> if list == [] then []
        else if tail list == [] then f (head list) []
        else f (head list) $ this this f (tail list))
        (\this f list -> if list == [] then []
        else if tail list == [] then f (head list) []
        else f (head list) $ this this f (tail list))
        f list)
-}
bubblesort :: Ord a => [a] -> [a]
bubblesort = foldl2 insert

--foldl2' = let l = (\this f list -> if list == [] then [] else if tail list == [] then f (head list) [] else f (head list) $ this this f (tail list)) in l l

--bubblesort' :: Ord a => [a] -> [a]
--bubblesort' = let l = (\this f list -> if list == [] then [] else if tail list == [] then f (head list) [] else f (head list) $ this f (tail list)) in (\y -> l l (\x list -> if list == [] then [x] else let y = head list; ys = tail list in if x < y then x:y:ys else y:(insert x ys)) 

listgen :: a -> (a -> Bool) -> (a -> a) -> [a]
listgen start condition change = if condition start then [] else start : (listgen (change start) condition change)
forlist :: a -> (a -> Bool) -> (a -> a) -> (a -> b) -> ([b] -> [c]) -> [c]
forlist start condition change toeach toall = toall . (map toeach) $ (listgen start condition change)