data Clist a = EnptyCL | CUnit a |Consnoc a (Clist a) a deriving (Show)

{-a-}

headCL :: Clist a -> a
headCL (CUnit a) = a
headCL (Consnoc a _ _) = a

{-b-}

tailCL :: Clist a -> Clist a
tailCL (CUnit a) = EnptyCL
tailCL (Consnoc _ xs _) = xs

{-c-}   

isEmptyCL :: Clist a -> Bool
isEmptyCL EnptyCL = True
isEmptyCL _ = False

{-d-}

isCUnit :: Clist a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

{-e-}

reverseCL :: Clist a -> Clist a
reverseCL EnptyCL = EnptyCL
reverseCL (CUnit a) = CUnit a
reverseCL (Consnoc a xs b) = Consnoc b (reverseCL xs) a

{-f-}

initsCL :: Clist a -> Clist (Clist a)
initsCL EnptyCL = CUnit EnptyCL
initsCL (CUnit a) = CUnit (CUnit a)
initsCL (Consnoc a xs b) = Consnoc EnptyCL (mapCL (Consnoc a) (initsCL xs)) (Consnoc a xs b)

mapCL :: (a -> b) -> Clist a -> Clist b
mapCL f EnptyCL = EnptyCL
mapCL f (CUnit a) = CUnit (f a)
mapCL f (Consnoc a xs b) = Consnoc (f a) (mapCL f xs) (f b)

{-g-}

lastsCL :: Clist a -> Clist a
lastsCL EnptyCL = EnptyCL   
lastsCL (CUnit a) = CUnit a
lastsCL (Consnoc a xs b) = Consnoc a (lastsCL xs) b

{-h-}

concatCL :: Clist a -> Clist a -> Clist a
concatCL EnptyCL ys = ys
concatCL xs EnptyCL = xs
concatCL xs ys = Consnoc (headCL xs) (tailCL xs) (headCL ys)

 
