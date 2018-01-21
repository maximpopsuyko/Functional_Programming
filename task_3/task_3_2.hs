{- Дана следующая структура данных:
data ReverseList a = RNil | RCons (ReverseList a) a
представляющая собой перевёрнутый односвязный список элементов типа a. Реализуйте функции перевода данного представления в обычные списки, а также экземпляры классов Eq, Ord, Show, Monoid и Functor. Конструкцию deriving использовать, разумеется, нельзя.
-}

module Task32 where 
data ReverseList a = RNil | RCons (ReverseList a) a

-- Преобразование из ReverseList в обычный список
revListToList RNil         = []
revListToList (RCons xs x) = (revListToList xs) ++ [x]

-- Преобразование из обычно списка в ReverseList
listToRevList [] = RNil
listToRevList x  = RCons (listToRevList $ init x) (last x)

---- Реализуем экземпляры
-- Экземпляр класса для Show
instance (Show a) => Show (ReverseList a) where
    show xs = show (revListToList xs)
	
-- Экземпляр класса равенства
instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) _    RNil = False
    (==) RNil _    = False
    (==) (RCons xs1 x1) (RCons xs2 x2) = if xs1 == xs2
                                         then if x1 == x2
                                              then True
                                              else False
                                         else False
										 
-- Экземпляр класса упорядочивания
instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil _ = True
    (<=) _ RNil = False
    (<=) (RCons xs1 x1) (RCons xs2 x2) = if x1 <= x2
                                         then True
                                         else if xs1 <= xs2
                                              then True
                                              else False

-- Экземпляр Моноида
instance Monoid (ReverseList a) where
    mempty                 = RNil
    mappend RNil x         = x -- в данном случае на входе имеем два моноида - revListToList (RNil) и x
    mappend x RNil         = x
    mappend x (RCons ys y) = RCons (mappend x ys) y -- на входе два моноида - revListToList. (RCons ys y) - где ys остаточный список revListToList, а y - значение. Для их объединения создаем новый RCons, в котором значением будет y, а остаточный список объединение x и ys - (mappend x ys)
	
-- Экземпляр Функтора
instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap fun (RCons xs x) = RCons (fmap fun xs) (fun x) -- Создаем RCons, в котором значением является (fun x), а остаточный список (fmap fun xs)


