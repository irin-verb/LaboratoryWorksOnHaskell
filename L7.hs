module L7 where

-- Факториал числа с явным аккумулятором





-- Реализуйте итерационное выражение суммы ряда (с использованием аккумулятора): 1+2+3+..+n
summa :: Integer -> Integer
summa 0 = 0
summa n = helper 0 n where
	helper s 0 = 0
	helper s i = helper (s+i) (i-1)






-- Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности:
-- а0 = 1, а1 = 2, а2 = 3, а k+3 = а k+2 + a k+1 - 2 a k
seqA :: Integer -> Integer
seqA n = 
	let helper n k fk2 fk1 fk
		| n < 3 = n + 1
		| n == k = fk
		| otherwise =  helper n (k+1) fk1 fk (fk + fk1 - 2*fk2)
	in helper n 2 1 2 3

seqA' :: Integer -> Integer
seqA' n
	| n < 3 = n + 1
	| otherwise = 
		let	
			helper a b c 2 = c
			helper a b c n = helper b c (c + b - 2*a) (n-1)
		in helper 1 2 3 n

seqA'' n = 
	let 
		helper n a b c = if n == 0 then a else helper (n-1) b c (c+b-2*a)
	in helper n 1 2 3
	
	
	
	
	
	
	
-- Проверка простоты натурального числа
-- вспомогат функция проверяет простоту при условии, что уже проверена его делимость на все числа, меньше заданного
isprime' :: Integer -> Integer -> Bool

isprime :: Integer -> Bool
isprime p 
	| p <= 0 = error "prime: Non-positive arg"
	| p == 1 = False
	| otherwise = isprime' p

-- рассм 3 случая, заданные тремя уравнениями
-- не найдено делителей числа, не превышающих его корня
-- найден делитель - число не простое
-- делитель пока не найден, продолжаем проверку
isprime' d p
	| d * d > p = True
	| p `mod` d == 0 = False
	| otherwise = isprime' (d+1) p
	
	
	
	
	

-- НОД 2ух чисел
nod :: Integer -> Integer
nod a 0 = a
nod a b
	| a < 0  = error "err"
	| b < 0 = error "err"
	| otherwise = nod b (a `mod` b)
	
	
	
	
	

-- Определение суммы ряда для получения приближенного значения числа е

e' :: Double -> Double -> Double -> Double -> Double
e' eps n mem sum
	| mem < eps = sum
	| otherwise = e' eps (n+1) (mem/n) (sum+mem)

e :: Double -> Double
e eps = e' eps 1 1 0



