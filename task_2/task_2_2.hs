--foldl
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f x [] = x
foldl' f x (head:list) = foldl f (f x head) list

--foldr
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f x [] = x
foldr' f x (head:list) = f head (foldr f x list)

--map
map' :: (a -> b) -> [a] -> [b]
map' f a = foldr' (\n list -> (f n) : list) [] a

--flatMap
flatMap' :: (a -> [b]) -> [a] -> [b]
flatMap' f a = foldr (\n list -> (f n) ++ list) [] a

--concat
concat' :: [a] -> [a] -> [a]
concat' a b = foldl' (\list n -> list ++ [n]) a b

--filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' f a = foldl' (\list n -> list ++ (if f n then [n] else [])) [] a

--maxBy
maxBy' :: (a -> Integer) -> [a] -> a
maxBy' f a@(n:list) = foldl' (\x y -> if f x > f y then x else y) n a
 
--minBy
minBy' :: (a -> Integer) -> [a] -> a
minBy' f a@(n:list) = foldl' (\x y -> if f x < f y then x else y) n a

--reverse
reverse' :: [a] -> [a]
reverse' a = foldl' (\list n -> n : list) [] a

--elementAt 
elementAt' :: Integer -> [a] -> a
elementAt' index (head:list) = snd
                                 (
                                     foldl'
                                          (
                                               \(i, x) y -> if i < 0
                                                            then (i-1, x)
                                                            else (i-1, y)
                                          )
                                          (index-1, head)
                                          list
                                 )
								
--indexOf
indexOf' :: String -> [String] -> Integer
indexOf' str list = if snd y then fst y else error "not found" where
                                y = foldl'
                                    (\x n -> if snd x == False
                                        then if str == n
                                             then (fst x, True)
                                             else (fst x + 1, False)
                                        else x
                                    )
                                    (0, False)
                                    list
