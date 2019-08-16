module Crypto where
 
import Data.Char
 
import Prelude hiding (gcd)
 
{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).
 
We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}
 
-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption
 
gcd :: Int -> Int -> Int
gcd m n
  | n == 0    = m
  | otherwise = gcd n (m `mod` n)
 
phi :: Int -> Int
phi m
  = length [a | a <- [1..m], gcd a m == 1]
 
--
-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
--
extendedGCD :: Int -> Int -> ((Int, Int), Int)
extendedGCD a b
  | b == 0      = ((1, 0) , a)
  | otherwise   = ((v, u - q * v), g)
  where
    (q, r)      = quotRem a b
    ((u, v), g) = extendedGCD b r
 
-- Inverse of a modulo m
inverse :: Int -> Int -> Int
inverse a m
  = u `mod` m
  where ((u, _), _) = extendedGCD a m
 
-- Calculates (a^k mod m) avoiding integer overflow
-- 
modPow :: Int -> Int -> Int -> Int
modPow a k m
  | k == 0         = 1 `mod` m
  | k `mod` 2 == 0 = recur `mod` m
  | otherwise      = ((a `mod` m) * (recur `mod` m) `mod` m) `mod` m
  where
    recur = modPow (a^2 `mod` m) (k `div` 2) m
 
-- Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf phi
  = head [b | b <- [1..(2*phi)], b>1, gcd phi b == 1]
 
-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q
  = ((e, n), (d, n))
  where
    n = p * q
    e = smallestCoPrimeOf ((q - 1) * (p - 1))
    d = inverse e ((q - 1) * (p - 1))
 
-- RSA encryption/decryption; (e, n) is the public key
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt m (e, n)
  = modPow m e n
 
rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d, n)
  = modPow c d n
 
-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption
 
-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt a
  | ord a >= ord 'a' && ord a <= ord 'z' = ord a - ord 'a'
  | otherwise                            = error "The input is not a letter."
 
-- Returns the n^th letter
toChar :: Int -> Char
toChar n
  | n >= 0 && n < 26 = chr (n + ord 'a')
  | otherwise        = error "The alphabet has 26 letters ordered from 0 to 25."
 
-- "adds" two letters
add :: Char -> Char -> Char
add a b
  = toChar ((toInt a + toInt b) `mod` 26)
 
-- "substracts" two letters
substract :: Char -> Char -> Char
substract a b
  = toChar ((toInt a - toInt b) `mod` 26)
 
-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"
 
-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt key m
  | m == []   = []
  | otherwise = c : ecbEncrypt key t
  where
    c = add (head m) key
    t = tail m
 
ecbDecrypt :: Char -> String -> String
ecbDecrypt key m
  | m == []   = []
  | otherwise = c : ecbDecrypt key t
  where
    c = substract (head m) key
    t = tail m
 
-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt key iv m
  | m == []   = []
  | otherwise = c : cbcEncrypt key c t
  where
    c = add (add (head m) iv) key
    t = tail m
 
cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt key iv m
  | m == []   = []
  | otherwise = c : cbcDecrypt key (head m) t
  where
    c = substract (substract (head m) key) iv
    t = tail m
