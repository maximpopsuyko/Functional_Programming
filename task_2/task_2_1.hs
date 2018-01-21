--Реализуйте структуру данных "бинарное дерево поиска".

data BinaryTree = EmptyTree | LeafTree Integer | Node Integer BinaryTree BinaryTree deriving (Show)

-- BinaryTree - пустое дерево
-- LeafTree Integer - лист дерева
-- Node Integer BinaryTree BinaryTree - узел, включающий левое и правое дерево

-- Добавления элемента:
insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = LeafTree x
insert (LeafTree value) x | x < value = Node value (LeafTree x) EmptyTree
                       | x > value = Node value EmptyTree (LeafTree x)
                       | otherwise = LeafTree value
insert (Node value leftTree rightTree) x | x < value = Node value (insert leftTree x) rightTree
                                         | x > value = Node value leftTree (insert rightTree x)
                                         | otherwise = Node value leftTree rightTree

--Удаления элемента: 
remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree     _ = EmptyTree
remove (LeafTree value) x | value == x    = EmptyTree        
                          | otherwise     = LeafTree value 
remove (Node value leftTree rightTree) x | x < value = Node value (remove leftTree x) rightTree
                                         | x > value = Node value leftTree (remove rightTree x)
                                         | otherwise = concatTree leftTree rightTree
                                         where concatTree EmptyTree    tree = tree
                                               concatTree (LeafTree v)    tree = Node v EmptyTree tree
                                               concatTree (Node v l r) tree = Node value l (concatTree r tree)

-- Создания пустого дерева:
emptyTree :: BinaryTree
emptyTree = EmptyTree

-- Поиска элемента в дереве:
containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree                          _ = False
containsElement (LeafTree value)                   x = x == value
containsElement (Node value leftTree rightTree) x | x < value = containsElement leftTree x
                                                  | x > value = containsElement rightTree x
                                                  | otherwise = True

-- Поиска в дереве наименьшего элемента, который больше или равен заданному:
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree x = undefined
nearestGE (LeafTree value) x | x == value = value
                             | otherwise = undefined
nearestGE (Node value leftTree@(EmptyTree) rightTree) x | x <= value = value
                                                        | otherwise = nearestGE rightTree x
nearestGE (Node value leftTree@(LeafTree leftValue) rightTree) x | x == value = value
                                                                 | x < value = if x > leftValue then value
                                                                                                else nearestGE leftTree x
                                                                 | otherwise = nearestGE rightTree x
nearestGE (Node value leftTree@(Node leftValue leftleftTree leftrightTree) rightTree) x | x == value = value
                                                                                        | x < value = if x > leftValue then value
                                                                                                                       else nearestGE leftTree x
                                                                                        | otherwise = nearestGE rightTree x

-- Создания дерева из списка:
treeFromList :: [Integer] -> BinaryTree
treeFromList list = foldl insert EmptyTree list

-- Создания списка из дерева:
listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree                       = []
listFromTree (LeafTree value)                = [value]
listFromTree (Node value leftTree rightTree) = [value] ++ (listFromTree leftTree) ++ (listFromTree rightTree)
