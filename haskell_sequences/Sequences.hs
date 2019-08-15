module Sequences where

import Data.Char (ord, chr)

maxOf2 :: Int -> Int -> Int
-- Returns first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 x y
  | x > y = x
  | otherwise = y

maxOf3 :: Int -> Int -> Int -> Int
-- Returns the largest of three Ints
maxOf3 x y z
  | maxOf2 x y < z = z
  | otherwise = maxOf2 x y

isADigit :: Char -> Bool
-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit c
  | ord c > 47 && ord c < 58 = True
  | otherwise = False

-- False otherwise
isAlpha :: Char -> Bool
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
isAlpha c 
  | ord c > 64 && ord c < 91 = True
  | ord c > 96 && ord c < 123 = True
  | otherwise = False

digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt c 
  | isADigit c == True = ord c - ord '0'
  | otherwise = error "The input character is not a digit."

toUpper :: Char -> Char
-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper c 
  | isAlpha c == True && ord c > 96 = chr (ord c - 32)
  | isAlpha c == True && ord c < 91 = c
  | otherwise = error "The input character is not a letter."

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n = a + d * (fromIntegral n)

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n = a * r ^ (fromIntegral n)

-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n = (term+1)*(a+d*term/2)
  where term = fromIntegral n

-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n
  | r == 1 = a*(term+1)
  | otherwise = a*((1-r^(n+1))/(1-r))
  where term = fromIntegral n
