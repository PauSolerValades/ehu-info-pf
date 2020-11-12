--- BINARY SEARCH TREES
data BST a = EmptyBST | NodeBST (BST a) a (BST a) deriving (Eq, Ord)
---------------------------------------------------
instance (Show a) => Show (BST a) where
         show EmptyBST = ""
         show t   = show' t 0 (maxLong t + 1)
                    where 
                    maxLong :: (Show a) => BST a -> Int
                    maxLong EmptyBST = 0
                    maxLong (NodeBST ai r ad) 
                         = maximum [maxLong ai,length(show r),maxLong ad]
                    show' :: (Show a) => BST a -> Int -> Int -> String
                    show' EmptyBST _ _ = " "
                    show' (NodeBST ai r ad) desde_col long_nodo
                      = dibujo_ai ++ "\n" ++ dibujo_raiz ++ dibujo_ad
                        where 
                        dibujo_raiz = [' '|i<-[1..desde_col]] ++ show r
                        dibujo_ai  = show' ai (desde_col + long_nodo) long_nodo
                        dibujo_ad = show' ad (desde_col + long_nodo) long_nodo

-----------------------------------------------------------------

createTree :: [a] -> BST a
createTree [] = EmptyBST
createTree xs = NodeBST (createTree fst(partit)) (xs !! meitat) (createTree snd(partit))
  where
    meitat = div length.xs 2
    partit = splitAt (meitat) xs

createTree xs = NodeBST (createTree ai) r (createTree ad)
    where n = length xs
        (ai,r:ad) = splitAt (div n 2) xs


bst1 = createTree [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
bst2 = createTree [1,3,5,7,9,11,13,15,17,19]
nobst = createTree [5,6,3,4,9,8,1,2,3,5,4,7,9]

-------------------------------------------------

isEmptyBST :: Eq a => BST a -> Bool
isEmptyBST t = (t == EmptyBST) --usando Eq

-- isBST :: Ord a => BST a -> Bool
                       
-- searchBST :: Ord a => a -> BST a -> Bool

-- insertBST :: Ord a => a -> BST a -> BST a

-- deleteBST :: Ord a => a -> BST a -> BST a
