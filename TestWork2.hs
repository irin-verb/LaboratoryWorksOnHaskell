module Main where

main = do 
 str <- getLine
 let result = delThirdChar str
 putStrLn result

delThirdChar :: [Char] -> [Char]
delThirdChar str = helper 0 str

helper :: Integer -> [Char] -> [Char]
helper _ [] = []
helper ind all@(x:xs)  
	| ind `mod` 3 == 0 = helper (ind + 1) xs
	| otherwise = x : helper (ind + 1) xs