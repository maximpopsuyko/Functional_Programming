-- Задание 3.1
{- Дана следующая структура данных:
data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber ) | Pred (WeirdPeanoNumber )
где Zero это 0, Succ X это (X + 1), Pred X это (X - 1).
Реализуйте для данного представления чисел все характерные для целых чисел классы типов. Проверьте работу реализованных функций.
-}

module Task31 where 
data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber) | Pred (WeirdPeanoNumber)

-- Перевод из WeirdPeanoNumber в целое
weirdPeanoToInt :: WeirdPeanoNumber -> Integer
weirdPeanoToInt Zero     = 0
weirdPeanoToInt (Succ a) = (weirdPeanoToInt a) + 1
weirdPeanoToInt (Pred a) = (weirdPeanoToInt a) - 1

-- Перевод из целого в WeirdPeanoNumber
intToWeirdPeano :: Integer -> WeirdPeanoNumber
intToWeirdPeano a | a > 0     = Succ (intToWeirdPeano (a - 1))
                  | a < 0     = Pred (intToWeirdPeano (a + 1))
                  | otherwise = Zero

------ Реализуем экземпляры классов типа WeirdPeanoNumber
-- Экземпляр класса равенства
instance Eq WeirdPeanoNumber where
    (==) Zero Zero         = True
    (==) (Succ a) (Succ b) = a == b
    (==) (Pred a) (Pred b) = a == b
    (==) _        _        = False 
-- "Не равно" определится автоматически

-- Экземпляр класса упорядочивания
instance Ord WeirdPeanoNumber where
    (<=) Zero (Pred _)     = False
    (<=) Zero _            = True
    (<=) (Succ a) (Succ b) = a <= b
    (<=) (Succ a) _        = False
    (<=) (Pred a) (Pred b) = a <= b
    (<=) (Pred a) _        = True
-- >= - не реализуем т.к. в классах >= и <= определены друг через друга

-- Экземпляр класса отображения строки в консоль
instance Show WeirdPeanoNumber where
    show a = show $ weirdPeanoToInt a
	
-- Экземпляр класса арифметических операций
instance Num WeirdPeanoNumber where

    (+)	Zero     a        = a
    (+) a        Zero     = a
    (+) (Succ a) (Succ b) = Succ $ (Succ a) + b
    (+) (Succ a) (Pred b) = a + b
    (+) (Pred a) (Pred b) = Pred $ (Pred a) + b
    (+) (Pred a) (Succ b) = a + b

    (*) Zero     _        = Zero
    (*) _        Zero     = Zero
    (*) (Pred a) (Pred b) = Succ $ a * b - a - b
    (*) (Pred a) (Succ b) = Pred $ a * b + a - b
    (*) (Succ a) (Pred b) = Pred $ a * b - a + b
    (*) (Succ a) (Succ b) = Succ $ a * b + a + b

    abs (Pred a) = Succ (abs a)
    abs a = a

    signum Zero     = Zero
    signum (Succ _) = Succ Zero
    signum (Pred _) = Pred Zero
	
    fromInteger a | (a == 0) = Zero
                  | (a > 0)  = Succ $ fromInteger (a - 1)
                  | (a < 0)  = Pred $ fromInteger (a + 1)
				  
    negate Zero     = Zero
    negate (Pred a) = Succ $ negate a
    negate (Succ a) = Pred $ negate a
	
-- Экземпляр класса действительных чисел
instance Real WeirdPeanoNumber where
    toRational Zero     = toRational 0
    toRational (Succ a) = toRational a + 1
    toRational (Pred a) = toRational a - 1
	
--Экземпляр класса перечисления
instance Enum WeirdPeanoNumber where
    toEnum a | (a == 0)	= Zero
             | (a > 0)  = Succ $ toEnum (a - 1)
             | (a < 0)  = Pred $ toEnum (a + 1)

    fromEnum Zero     = 0
    fromEnum (Succ a) = fromEnum a + 1
    fromEnum (Pred a) = fromEnum a - 1
	
-- Экземпляр класса целых чисел
instance Integral WeirdPeanoNumber where
    toInteger Zero     = 0
    toInteger (Pred a) = toInteger a - 1
    toInteger (Succ a) = toInteger a + 1
	
-- Деление с остатком. Возвращает два элемента: целая часть, остаток					  
    quotRem Zero _ = (Zero, Zero)
    quotRem _ Zero = error "Division by zero"
    quotRem a b | signum a == signum b = absValue -- случай: (Succ Succ) или (Pred Pred)
                | otherwise = (negate $ fst absValue, (signum a) * (snd absValue)) -- случай: (Succ Pred) или (Pred Succ) -- отрицаем результат целого числа и выбираем знак для остатка
                    where remSubdiv x@(integ, rem) div | rem >= div  = remSubdiv (integ + 1, rem - div) div
                                                       | otherwise = x 
                          absValue = remSubdiv (Zero, abs a) (abs b)
