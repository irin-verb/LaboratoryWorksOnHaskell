module L6 where

-- LET и WHERE -выражения

-- Определение площади треугольника по формуле Герона
geron' a b c = sqrt(p * (p - a) * (p - b) * (p - c))
	where 
		p = (a + b + c) / 2
	
geron'' a b c = sqrt (p * pa * pb * pc)
	where 
		p = (a + b + c) / 2
		pa = p - a
		pb = p - b
		pc = p - c

mult2 :: Int -> Int
mult2 x = mult (mult x)
	where 
		mult :: Int -> Int
		mult x = x * 2
		
--typeTriangle :: Double -> Double -> Double -> String
--typeTriangle a b c
	-- | a == b && b == c = "Ravnostoronn"
	-- | (ab == c * c || ac == b * b || bc == a * a) && (a == b || b == c || a == c) = "Prjamoug + Ravnobedr" 
	-- | a == b || b == c || a == c = "Ravnobedr"
	-- | ab = c * c || ac = b * b || bc = a * a = "Prjamoug"
	-- | otherwise = "Proizvoln"
	-- where 
		-- ab = a * a + b * b
		-- ac = a * a + c * c
		-- bc = b * b + c * c
		
ger :: Double -> Double -> Double -> Double
ger a b c =
	let
		p = (a + b + c) / 2
		pa = p - a
		pb = p - b
		pc = p - c
	in sqrt(p * pa * pb * pc)

