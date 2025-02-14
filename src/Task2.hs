{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- The above pragma enables all warnings
-- (except for unused imports from Task1)

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)

-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden
import Task1 (reverse, map, sum, doubleEveryOther, toDigits, luhn)

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1


normalizeN :: Int -> Int -> Int
normalizeN m n
    | n > (m - 1) = n - (m - 1)
    | otherwise = n


luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN m f n = (m - (sum (map (normalizeN m) (doubleEveryOther (reverse (map f n))))) `mod` m) `mod` m

-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec n = luhnModN 10 id n

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15

luhnHex :: [Char] -> Int
luhnHex n = luhnModN 16 digitToInt n

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt n
    | '0' <= n && n <= '9' = fromEnum n - fromEnum '0'
    | n=='A' || n=='a' = 10
    | n=='B' || n=='b' = 11
    | n=='C' || n=='c' = 12
    | n=='D' || n=='d' = 13
    | n=='E' || n=='e' = 14
    | n=='F' || n=='f' = 15
    | otherwise = error "Wrong char is given"

-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec n
    | luhnDec (toDigits (n `div` 10)) == ((fromIntegral n) `mod` 10) = True
    | otherwise = False


getLastChar :: [Char] -> Char
getLastChar [] = error "Empty string!"
getLastChar [x] = x
getLastChar (_:xs) = getLastChar xs


getPrefix :: [Char] -> [Char]
getPrefix [] = error "Empty string!"
getPrefix [_] = []
getPrefix (x:xs) = x : getPrefix xs


-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

validateHex :: [Char] -> Bool
validateHex n
    | luhnHex (getPrefix n) == (digitToInt (getLastChar n)) = True
    | otherwise = False
