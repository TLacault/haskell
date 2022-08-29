data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Show)

addInTree :: Ord a => a -> Tree a -> Tree a
addInTree a (Node left mid right)
    | mid > a = Node (addInTree a left) mid right
    | otherwise = Node left mid (addInTree a right)
addInTree a Empty = Node Empty a Empty