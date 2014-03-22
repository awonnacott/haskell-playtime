module RSA
( modexp
, encode
, decode
) where

-- RSA version 2.0

modexp :: Integral a => a -> a -> a -> (Maybe a) -> a -- modular exponentiation
modexp b e m (Just hint) = modexp b (e mod hint) m Nothing
modexp b e m Nothing
	| e == 0 = 1
	| b <  0 = modexp (mod b m) e m Nothing
	| b >= m = modexp (mod b m) e m Nothing
	| e <  0 = modexp b (mod e m) m Nothing
	| e == 1 = b
    | odd  e = mod ((modexp b (e - 1) m Nothing) * b) m
    | even e = mod ((modexp b (div e 2) m Nothing)^2) m
-- compute inverse of a mod p*q using Euler's theorem
inverse :: Integral a => a -> a -> a -> a
-- assumes prime p, prime q, when a <\- {1,p,q,p*q}
inverse a p q = modexp a ((p-1)*(q-1)-1) (p*q) ((p-1)*(q-1))
encode :: Integral a => a -> a -> a -> a
encode n e x = modexp x e n Nothing
decode :: Integral a => a -> a -> a -> a -> a
decode p q e x = modexp x (inverse e p q) (p*q) ((p-1)*(q-1))

{-
-- start with primes p and q
p = 
q = 
n = p*q
-- use (p-1)*(q-1) for phi(n)
--e is relatively prime with phi(n)
e =
-- d * e = 1 mod phi(n)
-- so d = e^-1 mod phi(n)
-- compute e^phi(n)-1
-- modular exponentiate e by ((p-1)(q-1)-1) modulo n
d = modexp e ((p-1)*(q-1)-1) n
encode x = modexp x e n
decode x = modexp x d n
-}