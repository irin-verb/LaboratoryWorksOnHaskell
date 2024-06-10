module L8 where


-- 7 глава "Создание новых типов и классов типов" страницы 148-190 

data MyType = 1 | 2 | 3
data Sex = Male | FEmale deriving (Show, Eq)
-- deriving - явл представителем класса
data MyInt = -100 | -90 | ... | -1 | 0 | 1 | 2 | ... | 90 | 100
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- 6 глава "Модули" страницы 122-145
-- Создать свою иерархию модулей (воины, люди, животные)


{-- 	
	Многострочный
	комментарий! 
--}

