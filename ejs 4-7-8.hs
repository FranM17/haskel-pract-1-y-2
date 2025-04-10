data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving (Show, Eq)
{-Defina un evaluador eval :: Aexp → Int. ¿C´omo maneja los errores de divisi´on por 0?-}
eval :: Aexp -> Int
eval (Num n) = n
eval (Prod a b) = eval a * eval b
eval (Div a b) = if eval b == 0 then 0 else eval a `div` eval b


safeEval :: Aexp -> Maybe Int
safeEval (Num n) = Just n
safeEval (Prod a b) = case (safeEval a, safeEval b) of
    (Just x, Just y) -> Just (x * y)
    _ -> Nothing
safeEval (Div a b) = case (safeEval a, safeEval b) of   
    (Just x, Just 0) -> Nothing
    (Just x, Just y) -> Just (x `div` y)
    _ -> Nothing




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

{-8-}
fromOrdList :: Ord a => [a] -> RBT a
fromOrdList [] = E
fromOrdList xs = go xs E
    where
        go [] t = t
        go (x:xs) t = go xs (insert x t)

{-7-}
{-La definici´on de member dada en teor´ıa (la cual determina si un elemento est´a en un bst)
realiza en el peor caso 2 ∗ d comparaciones, donde d es la altura del ´arbol. Dar una definici´on
de menber que realice a lo sumo d + 1 comparaciones.
Para ello definir member en t´erminos de
una funci´on auxiliar que tenga como par´ametro el elemento candidato, el cual puede ser igual al
elemento que se desea buscar (por ejemplo, el ´ultimo elemento para el cual la comparaci´on 
de
a 6 b retorn´o True) y que chequee que los elementos son iguales s´olo cuando llega a una hoja del
´arbol.
-}

member :: Ord a => a -> RBT a -> Bool
member a E = False
member a (T _ l b r)
                        | a == b = True
                        | a < b = member a l
                        | a > b = member a r

{-el peor caso es cuando no encontramos un nodo-}


menAux :: Ord a => a -> RBT a -> Bool   
menAux a E = False
menAux a (T _ l b r)
    | a < b =  menAux a l
    | otherwise =  menAux' b a r
    where
        menAux' candidate a E = candidate == a
        menAux' candidate a (T _ l b r)
            | a < b =  menAux' candidate a l
            | otherwise =  menAux' b a r


men c x E = c == x
men c x (n l y r)
                | x>y = men c x r
                |otherwise = men y x l 

{-memeber x t@(n l c r) = men c x t-}
{-este alg se usa cuando tenemos mas casos de eementos q no estan que
elementos q si estan, mejorar el peor caso.cuando la mejora es por una cte
no hay taaaaaaaanta mejora, no es mejora sustancia, funciona cuando la altura 
es pequeña, no es un arbol muy grande. -}