type RGB = (Int, Int, Int)

mezclar :: RGB -> RGB -> RGB
mezclar  (r1, g1, b1) (r2, g2, b2) = (div (r1+r2) 2, div (g1+g2) 2, div (b1+b2) 2)

data Color = Color  {red:: Int
                    , green :: Int
                    , blue :: Int} deriving (Show)

mez :: Color-> Color-> Color
mez (Color r1 g1 b1) (Color r2 g2 b2) = (Color (div (r1+r2) 2)(div (g1+g2) 2)(div (b1+b2) 2))



type Linea = ( [Char], Int)

vacia :: Linea 
vacia = ([], 0)  

movIzq :: Linea -> Linea
movIzq  (xs, c) = if c>0 then (xs, (c-1)) else (xs, c)

movDer :: Linea -> Linea
movDer (xs, c) = if (length xs 0) < c then (xs, c+1) else (xs, c)

movIni :: Linea -> Linea
movIni (_, c) = (_, 0)

movFin :: Linea -> Linea
movFin (xs, c) =  (xs, (length xs 0))

insert :: Char -> Linea -> Linea
insert a (xs, c) = ((ins a c xs), c+1)

ins c 0 cs = c:cs
ins c p (x:cs) = x :ins c (p-1) cs


{- type linea = ([Char], [Char])

inser :: Char -> Linea -> Linea {-costo cte-}
inser a (ls, rs) = (a:ls, rs)

movIzq ("", rs) = ("", rs)
movIzq (c:ls, rs) = (ls, c:rs) 

movDer (ls, []) = (ls, [])
movDer (ls, c:rs) = (c:ls, rs) {-te ahorras los length-}

movIni (ls, rs) = ([], rev ls []++rs)

movFin (ls, rs) = ([], rev ls []++rs)


{- cual sera mejor?

se mide la funcion de uso mas frecuente, en este caso es mas facil insertar, por ende
es mas factible usar la con insert
-}

Tarea, seguir con la practica

-}
