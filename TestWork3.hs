module Main where

main = do 
 a <- getLine
 b <- getLine
 c <- getLine
 d <- getLine
 let x1 = read a::Double
 let x2 = read b::Double
 let y1 = read c::Double
 let y2 = read d::Double
 let result = show (distance x1 x2 y1 y2)
 putStrLn result

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x1 - x2)^2 + (y1 - y2)^2)