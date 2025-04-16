{-La funci´on insert dada en teor´ıa para insertar un elemento en un rbt puede optimizarse
elimando comparaciones innecesarias hechas por la funci´on balance. Por ejemplo, en la definici´on
de la funci´on ins cuando se aplica balance sobre el resultado de aplicar insert x sobre el sub´arbol
izquierdo (l) y el sub´arbol derecho (r ), los casos de balance para testear que se viola el invariante
1 en el sub´arbol derecho no son necesarios dado que r es un rbt.
a) Definir dos funciones lbalance y rbalance que chequeen que el invariante 1 se cumple en los
sub´arboles izquierdo y derecho respectivamente.-}

insert :: Ord a => a → Bin a → Bin a
insert a Hoja = Nodo Hoja a Hoja
insert a (Nodo l b r ) 
        | a 6 b = Nodo (insert a l) b r
        | otherwise = Nodo l b (insert a r )