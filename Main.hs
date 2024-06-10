module Main where

main = do 
 number <- getLine
 let num = read number::Integer
 let result = show (minPrimeDivider num)
 putStrLn result

minPrimeDivider :: Integer -> Integer
minPrimeDivider x = helper x 2

helper :: Integer -> Integer -> Integer
helper number divider
 | isprime number = number
 | number `mod` divider == 0 && isprime divider = divider
 | otherwise = helper number (divider+1)

isprime' :: Integer -> Integer -> Bool
isprime' d p
 | d*d > p = True
 | p `mod` d == 0 = False
 | otherwise = isprime' (d+1) p
 
isprime :: Integer -> Bool
isprime p
 | p == 1 = False
 | otherwise = isprime' 2 p
