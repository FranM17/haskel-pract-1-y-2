{-Definir una funci´on fromOrdList :: [a ] -> RBT a, que cree un red black tree a partir de una
lista ordenada sin elementos repetidos. La funci´on debe ser de orden O(n).
9. La funci´on insert dada en teor´ıa para insertar un elemento en un rb0-}
data Color = R | B deriving (Show, Eq)
data RBT a = E | T Color (RBT a) a (RBT a) deriving (Show, Eq)

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
    where 
        ins x E = T R E x E
        ins x (T c l y r)   
                            | x < y     = balance c (ins x l) y r
                            | x > y     = balance c l y (ins x r)
                            | otherwise = T c l y r

makeBlack :: RBT a -> RBT a
makeBlack E = E
makeBlack (T _ l x r) = T B l x r

fromOrdList :: Ord a => [a] -> RBT a
fromOrdList [] = E
fromOrdList xs = go xs E
    where
        go [] t = t
        go (x:xs) t = go xs (insert x t)
