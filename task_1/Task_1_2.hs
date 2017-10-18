-- Task 1.2
-- Наибольший общий делитель двух чисел
nod :: Integer -> Integer -> Integer
nod a b = if a == b then a
                    else if a > b then if a `mod` b == 0 then b 
                                                         else nod (a - b) b
                                  else if b `mod` a == 0 then a 
                                                         else nod a (b - a)

-- Существует ли натуральное число, являющееся квадратом другого числа, между двумя заданными целыми числами?
isSqrt :: Integer -> Bool
isSqrt n | n <= 0 = False
         | otherwise = if (foldl (\x y -> if x >= n then x else x + y) 0 [1, 3 .. n]) == n then True
		                                                                                   else False
isSqrtFound :: Integer -> Integer -> Bool
isSqrtFound a b = foldl (\x y -> x || (isSqrt y)) False [a, a+1 .. b]

-- Является ли заданная дата (число, месяц, год) корректным числом с учётом високосных годов и количества дней в месяце?
isRealDate :: Int -> Int -> Int -> Bool
isRealDate day month year = if day < 1 || month < 1 || year < 1 || month > 12 then False
                                                                              else if month == 2 then if year `mod` 4 == 0 then day <= 29 else day <= 28
                                                                                                 else day <= monthDaysNum!!(month - 1)
                                                                                                 where monthDaysNum = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]