{-1) borrarUltimo que dada una lista borra el ´ultimo elemento de la lista. No utilizar reverse, ni
tail.-}

bUlt :: [a] -> [a]
bUlt [] = []
bUlt [x] = []
bUlt (x:xs) = x : bUlt xs

{-collect :: [(k, v)] → [(k, [v ])] toma 
un lista de pares (clave, valor ) 
y asocia cada clave ´unica
con todos los valores con los que estaba 
apareada originalmente.
Por ejemplo: collect [(3, 7),(2, 6),(1, 8),(3, 5),
(2, 5)] = [(3, [7, 5]),(2, [6, 5]),(1, [8])]-}

collect :: Eq k => [(k, v)] -> [(k, [v])]
collect [] = []
collect ((k, v):xs) = (k, [v] ++ [v' | (k', v') <- xs, k' == k]) 
                      : collect [(k', v') | (k', v') <- xs, k' /= k]