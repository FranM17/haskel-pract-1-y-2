data BST a = Empty | Node a (BST a) (BST a) deriving (Show, Eq)

maximumBST :: Ord a => BST a -> a
maximumBST Empty = error "El árbol está vacío"
maximumBST (Node val _ Empty) = val
maximumBST (Node _ _ right) = maximumBST right

checkBST :: Ord a => BST a -> Bool
checkBST tree = isBST tree Nothing Nothing
    where
    isBST :: Ord a => BST a -> Maybe a -> Maybe a -> Bool
    isBST Empty _ _ = True
    isBST (Node val left right) minVal maxVal =
        withinBounds val minVal maxVal &&
        isBST left minVal (Just val) &&
        isBST right (Just val) maxVal

    withinBounds :: Ord a => a -> Maybe a -> Maybe a -> Bool
    withinBounds val Nothing Nothing = True
    withinBounds val (Just minVal) Nothing = val > minVal
    withinBounds val Nothing (Just maxVal) = val < maxVal
    withinBounds val (Just minVal) (Just maxVal) = val > minVal && val < maxVal