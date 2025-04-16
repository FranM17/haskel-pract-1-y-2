{-1) borrarUltimo que dada una lista borra el ´ultimo elemento de la lista. No utilizar reverse, ni
tail.-}

bUlt :: [a] -> [a]
bUlt [] = []
bUlt [x] = []
bUlt (x:xs) = x : bUlt xs

{-collect :: [(k, v)] -> [(k, [v ])] toma 
un lista de pares (clave, valor ) 
y asocia cada clave ´unica
con todos los valores con los que estaba 
apareada originalmente.
Por ejemplo: collect [(3, 7),(2, 6),(1, 8),(3, 5),
(2, 5)] = [(3, [7, 5]),(2, [6, 5]),(1, [8])]-}

collect :: Eq k => [(k, v)] -> [(k, [v])]
collect [] = []
coll [] = []
coll ((k, v):xs) = (k, v:[v' | (k', v') <- xs, k' == k]) : coll [(k', v') | (k', v') <- xs, k' /= k]

{- serie que se comporta de la siguiente manera: 
serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]]
Dar su tipo m´as general.-}

serie :: [Int] -> [[Int]]
serie [] = [[ ]]
serie xs = serie (init xs) ++ [xs]                   

{-paresIguales :: Int -> Int -> Int -> Int -> Bool toma 
4 n´umeros enteros y retorna True si de
dos en dos son iguales (en cualquier orden),
 en los dem´as casos retorna False. Por ejemplo:
paresIguales 3 1 1 2 = False paresIguales 3 1 3 1 
= True paresIguales 3 3 1 1 = True
paresIguales 3 1 1 3 = True-}

pIg :: Int -> Int -> Int -> Int -> Bool 
pIg a b c d 
            |a==c && b == d = True
            | a == b && c==d = True
            |a==d && b==c = True
            | otherwise = False

{-ror que dada una lista xs y un entero n, 
tal que n 6 lenght xs, rota los primeros n elementos
de xs a la derecha: ror 3 [1, 2, 3, 4, 5] = 
    [4, 5, 1, 2, 3]. Definir una versi´on 
    recursiva de ror ,
sin usar drop, take ni tail.-}

ror :: [a] -> Int -> [a]
ror [] _ = []
ror xs x = go p xs xy 
    where
        p = xs !! x
        