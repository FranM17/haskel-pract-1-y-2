data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving (Show)

data Maybe a = Just a | Nothing deriving (Show)

{-a-}
eval :: Aexp -> Int
eval (Num n) = n
eval (Prod a b) = (eval a) * (eval b)
eval (Div a b) =  
    let divisor = eval b
    in if divisor == 0
        then 0 -- Manejo de división por cero  
        else div (eval a) divisor

{-b-}

seval :: Aexp -> Prelude.Maybe Int
seval (Num n) = Prelude.Just n
seval (Prod a b) = do
    x <- seval a    
    y <- seval b  
    return (x * y)
seval (Div a b) = do
    x <- seval a  
    y <- seval b 
    if y == 0
        then Prelude.Nothing  
        else Prelude.Just (div x y)

