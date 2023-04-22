
module Tree (Tree (Node), children,height,width,leafs) where


children :: Tree a -> [Tree a]
children (Node _ xs) = xs


height :: Tree a -> Int
height (Node _ []) = 1
height (Node _ xs) = 1 + maximum (height <$> xs)


width :: Tree a -> Int
width n = go [n]
  where
    go :: [Tree a] -> Int
    go [] = 0
    go (Node _ []:t) = 1 + go t
    go (Node _ (h:t):r) = go [h] + go t + go r
    

leafs :: Tree a -> [Tree a]
leafs n@(Node _ []) = [n]
leafs (Node _ xs) = concatMap leafs xs

data Tree a = Node a [Tree a]
