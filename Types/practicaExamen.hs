data Comp a = Comp a [a]-- amb data l'únic que li vols dir a haskell és com ha de guardar les dades

c1 = Comp 1.0 [-2.0, 2.0]
c2 = Comp 0.4 [2.3, 8.6, 3.4]
c3 = Comp 1.0 [-2.0, 2.0, 3.0]

instance Show a => Show (Comp a) where
    show a = mostrar a
        where
            mostrar (Comp r rs) = show r ++ "+" ++ mImag rs 'i'
            mImag::Show a => [a] -> Char -> String
            mImag (r:rs) s = if null rs then show r ++ [s] else show r ++ [s] ++ "+" ++ mImag rs (noxt s)
            noxt::Char -> Char
            noxt s = toEnum (fromEnum s + 1)

instance Eq a => Eq (Comp a) where
    a == b = igual a b
        where
            igual (Comp a as) (Comp b bs) = a==b && bs == as --les llistes tenen definit l'igual ja xD

sumLlista as bs = if length as >= length bs 
                  then let xs = zip as bs in [x+y | (x,y) <- xs]
                  else let xs = zip bs as in [x+y | (x,y) <- xs]

minLlista as bs = if length as >= length bs 
                  then let xs = zip as bs in [x+y | (x,y) <- xs]
                  else let xs = zip bs as in [x+y | (x,y) <- xs]

equal::(Eq a) => [a] -> [a] -> Bool
equal as bs = let xs = zip as bs in length as == length bs && and[x == y | (x,y) <- xs]

{-
instance Num a => Num (Comp a) where
    (Comp a as) + (Comp b bs) = (Comp a+b sumLlista as bs)
    (Comp a as) - (Comp b bs) = (Comp a+b minLlista as bs)
    (Comp a as) * (Comp b bs) = (Comp a*b)
-}

data Vector a = Vector [a] | Matriz [Vector a]

v1 = Vector [1,2,3]
v2 = Vector [4,5,6]
v3 = Vector [7,8,9]

m1 = Matriz [v1,v2,v3]
m2 = Matriz [v3,v2,v1]

instance Show a => Show (Vector a) where
    show a = case a of
        (Vector a)  -> "(" ++ ensenya a
        (Matriz a)  -> "(" ++ ensenyaMat a
        where
            ensenya::Show a => [a] -> String
            ensenya (a:as) = if null as then show a ++ ")" else show a ++ "," ++ ensenya as
            ensenyaMat::Show a => [a] -> String
            ensenyaMat (a:as) = if null as then show a ++ ")" ++ "\n" else show a ++ "\n" ++ ensenyaMat as

instance Eq a => Eq (Vector a) where
    Vector a == Vector b = a==b
    Matriz a == Matriz b = let xs = zip a b in and[x == y | (x,y)<-xs]
{-
-- a veure, tecnicament això és pot fer, però el compliador m'avisa de que no compleix les característiques d'una relació d'ordre, cosa evidentment necessaria per implementar ord.
instance (Floating a, Ord a) => Ord(Vector a) where
    a >= b = euclideanNorm a >= euclideanNorm b
-}

evaluar::Num a => Vector a -> Char -> Vector a -> Vector a
evaluar a ch b
    | ch == '+' = sumVector a b

    where
        sumVector::(Num a ) => Vector a -> Vector a -> Vector a
        sumVector (Vector a) (Vector b) = let xs = zip a b in Vector [x+y | (x,y)<-xs]

sumVectors as bs = if length as >= length bs 
                  then let xs = zip as bs in [x+y | (x,y) <- xs]
                  else let xs = zip bs as in [x+y | (x,y) <- xs]

euclideanNorm (Vector a) = sqrt(sum[x*x | x<-a])

newtype M a = M [Vector a] deriving (Eq)

m :: M Integer
m = M [v1,v2,v3]

instance Show a => Show (M a) where
    show a = show' a
        where
            --show':: M a -> String
            show' (M a) = aux [Vector a]
            aux vn = if null vn then show (head vn) ++ "prueba" else show (head vn) ++ "\n" ++ aux (tail vn)

--IMPLEMENTACIÓN DEL LOS NATURALES
data Nat = Zero | Suc Nat

n1 = Zero
n2 = Suc(Suc Zero)
n3 = Suc(Suc Zero)
n4 = Suc(Suc n3)
instance Show Nat where --Si no hi ha argument, no fa falta Show a => Show (Type a)
    show Zero = show 0
    show (Suc n) = show( showSucc (Suc n))
        where
            showSucc::Nat -> Integer
            showSucc (Suc n) = if n == Zero then 1 else 1 + showSucc n

countSuc::Nat->Int
countSuc Zero = 0
countSuc (Suc n) = 1 + countSuc n


instance Eq Nat where
    Zero == Zero = True
    Zero == (Suc _) = False
    (Suc _) == Zero = False
    (Suc n) == (Suc m) = n == m

instance Ord Nat where
    Zero < _ = True
    _ <= Zero = False --l'igual del menor o igual és important
    (Suc n) <= (Suc m) = n <= m

instance Num Nat where
    n + m = addNat n m
    n - m = subNat n m
    n * m = mulNat n m
    abs n = n
    signum n = if n==Zero then 0 else 1
    fromInteger i = createNat i

instance Enum Nat where
    succ n = Suc n
    pred (Suc n) = n
    pred Zero = error "No hi ha res més petit que 0"
    toEnum i = createNat (fromIntegral i)
    fromEnum n = countSuc n
    enumFrom n = iterate addSuc n
    enumFromThen n m = let p = n+m in iterate (+p) n
    enumFromTo n m = if n > m then [] else take (countSuc m) (iterate addSuc n)
    enumFromThenTo n m p
        | n < m       = []
        | p > n+m     = []
        | otherwise   = takeWhile (<=m) (iterate (+p) n)



addNat::Nat -> Nat -> Nat
addNat Zero n = n
addNat (Suc n) (Suc m) = addNat n (Suc(Suc m))

subNat::Nat -> Nat -> Nat
subNat n Zero = n
subNat (Suc n) (Suc m) = subNat n m
subNat n m = error $ show n ++ "-" ++ show m ++ " no és natural"

mulNat::Nat -> Nat -> Nat
mulNat _ Zero = Zero
mulNat (Suc n) m = aux (Suc n) m Zero
    where aux (Suc n) m acc = if n == Zero then addNat acc m else aux n m (addNat acc m)

createNat::Integer -> Nat
createNat i = if i==0 then addSuc Zero else addSuc (createNat (i-1))

addSuc :: Nat -> Nat
addSuc = Suc

{-
createNatInt::Int -> Nat
createNatInt i = if i==0 then addSuc Zero else addSuc (createNatInt (i-1))
-}


{-
--igualdad no miqueta matematica
instance Eq Nat where
    n == m = countSuc n == countSuc m

-- és una miqueta no matematic
instance Ord Nat where
    n <= m = countSuc n <= countSuc m

-}


{-
data Complejo a = Real a | Complejo a [a] deriving (Eq, Ord)

c1 = Real 1.0
c2 = Complejo 1.0 [1.0, 2.0]

instance Show a => Show (Complejo a) where
    show a = case a of
        (Real a) -> show a
        (Complejo r rs) -> show r ++ imgA rs 'i'
            where
                imgA::(Ord a)=> [a]-> Char -> String
                imgA (r:rs) c
                    | r >= 0    = if null rs then show r ++ [c] else show r ++ [c] ++ "+" ++ mImag rs (noxt c)
                    | r < 0    = if null rs then show r ++ [c] else "-" ++ show (abs r) ++ [c] ++ "-" ++ mImag rs (noxt c)
                noxt::Char -> Char
                noxt s = toEnum (fromEnum s + 1)
-}