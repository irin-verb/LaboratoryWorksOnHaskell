module Verbickaya4 where

-- 2 3 6 7 10 11

-- Задание 2
init' :: [Integer] -> [Integer]
init' all@(x:xs)
	| length all == 0  || length all == 1 = []
	| otherwise = x : init' xs

-- Задание 3
addElem :: [Integer] -> Integer -> [Integer]
addElem all@(x:xs) item 
	| length all == 1 = [x,item]
	| otherwise = x : addElem xs item

-- Задание 6
predicat :: Integer -> Bool
predicat x = x < 0

dropWhile' :: (Integer -> Bool) -> [Integer] -> [Integer]
dropWhile' _[] = []
dropWhile' pred all@(x:xs) 
	| pred x = dropWhile' pred xs
	| otherwise = all

-- Задание 10
seqA :: Integer -> Integer
seqA n =
	let helper n a b c 
		| n < 0 = error "Wrong argument!"
		| n == 0 = a
		| n == 1 = b
		| n == 2 = c
		| otherwise = helper (n-1) b c (a - 2*b + c)
	in helper n (-3) (-2) 1

-- Задание 11
isprime' :: Integer -> Integer -> Bool
isprime' d p
	| d*d > p = True
	| p `mod` d == 0 = False
	| otherwise = isprime' (d+1) p

isprime :: Integer -> Bool
isprime p
	| p <= 0 = error "!"
	| p == 1 = False
	| otherwise = isprime' 2 p

delNotPrime :: [Integer] -> [Integer]
delNotPrime [] = [] 
delNotPrime (x:xs) 
	| isprime x = x : delNotPrime xs
	| otherwise = delNotPrime xs

-- Доп. задание
-- :: [Integer] -> Integer -> Integer
--getByIndex [] n = error "List is empty!"
--getByIndex (x:xs) n 
	-- | n < 0 = error "Wrong index!"
	-- | otherwise = let 
				--	helper (x:xs) n acc == 0 = x
				--		| 
				--	in helper (xs) acc n