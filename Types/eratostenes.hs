import Data.Char (digitToInt)

--fibonacci NO eficiente
fib::Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--fibonacci SÍ eficiente
fibE::Integer -> Integer
fibE n = fst (fibPar n)
    where
        fibPar::Integer -> (Integer, Integer)
        fibPar 0 = (0,1) -- els dos primers elemtns de cop
        fibPar n = 
              let (a,b) = fibPar (n-1)
                  -- a = fib (n-1), b = fib n => a+b = fib n+1
              in (b, a+b)

--Ejercicio: CÓMO SE EVALUA fibE 3.
{-
fst(fibPar 3)
fst((b,a+b) where (a,b) = fibPar 2)
fst((b,a+b) where (a,b) = (b1, b1+a1) where (a1, b1) = fibPar 1)
fst((b,a+b) where (a,b) = (b1, b1+a1) where (a1, b1) = (b2, b2+a2) where (a2, b2) = fibPar 0))
fst((b,a+b) where (a,b) = (b1, b1+a1) where (a1, b1) = (b2, b2+a2) where (a2, b2) = (0,1))))
fst((b,a+b) where (a,b) = (b1, b1+a1) where (a1, b1) = (1, 1))
fst((b,a+b) where (a,b) = (1, 2))
fst((2,3))
2
-}

--EJERCICIO: iterate sacar fibonacci

fibIterate::Int->Integer
fibIterate n = fst (last (take (n+1) fibList))
    where
        fibList = iterate (\(x,y)->(y,x+y)) (0,1)

fibList::[Integer]
fibList = map fst (iterate (\(x,y)->(y,x+y)) (0,1))


--inversa ineficiente. tiene que hacerla recursión y tiene que volver otra vez.
inversa [] = []
inversa (x:xs) = inversa xs ++ [x]

--inversa si efinente (recursiva de cola)
inversaE = invConc []
    whereS
          invConc t [] = t
          invConc t (x:xs) = invConc (x:t) xs

-- Ejercicio con iterate
digitos = reverse . map (`mod` 10) .  takeWhile (/=0) . iterate (`div` 10) 

-- La criba de Eratostenes
primos = criba [2..]
criba (p : ns) = p : criba (filter ((/= 0).(`mod`p)) ns)

-- take 100 (filter ((==7).(`mod` 10)) primos) --los 100 primeros primos que acaban en 7
-- (filter ((==7).(`mod` 10)) primos) !! 99


-- Estructura cíclicas
data Arbol a = Hoja a | Nodo (Arbol a) (Arbol a) deriving Show

ca = let x =(Nodo (Nodo ca (Hoja 5)) x) 
     in Nodo x (Hoja 6)

ca' = let x =(Nodo (Nodo ca (Hoja 5)) x) 
      in Nodo (Hoja 6) x

unfold:: (Show a1, Eq a2, Num a2) => a2 -> Arbol a1 -> Arbol String
unfold _ (Hoja r) = Hoja (show r)
unfold 0 (Nodo ai ad) = Hoja " "
unfold n (Nodo ai ad) = Nodo (unfold (n-1) ai)  (unfold (n-1) ad)


--con repeat sacar [1,3,1,3,1,3,...] concat(repeat [1,3])

--ISBN www.isbn-check.com
{-
Sea i un ISBN correcto. 978-18-6197271-2. pasamos a numero
9781861972712

9·1 = 9
7·3 = 21
8·1 = 8
1·3 = 3
8·1 = 8
6·3 = 18


la suma da 118 =s

si m=(s%10==0), el 13ésimo dígito del ISBN debe ser cero.
En caso contrario, el 13ésimo dígito del ISBN debe ser 10-m
-}

isISBN::String->Bool
isISBN s = correctLength && correctFormula
    where
        isbnStr = filter (/='-') s
        correctLength = length isbnStr == 13
        digits = map digitToInt isbnStr
        suma = sum (zipWith (*) digits (cycle [1,3]))
        m = mod suma 10
        lastDigit = last digits
        correctFormula = if m == 0 then lastDigit == 0 
                         else lastDigit == 10-m
        
--usar filter, length, map, zipWith, init, last, [1,3,1,3,1,3,...]

-- Números de Hamming               
hamming :: [Integer] 
-- hamming es la lista infinita creciente de los números de Hamming
hamming = 1: fundir3 (map (2*) hamming)
                     (map (3*) hamming)
                     (map (5*) hamming)
          where fundir3 :: Ord a => [a] -> [a] -> [a] -> [a] 
                -- Pre: xs ys y zs son listas ordenadas crecientemente
                -- (fundir3 xs ys zs) es la lista ordenada creciente 
                --  sin repeticiones que resulta de mezclar xs, ys y zs
                fundir3 xs ys zs = fundir2 xs (fundir2 ys zs)
                fundir2 (x:xs) (y:ys)
                    | x == y = x: fundir2 xs ys     
                    | x < y  = x: fundir2 xs (y:ys)
                    | x > y  = y: fundir2 (x:xs) ys

{-
*Main> hamming !! 10000000
16244249195502759967226308067328000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
(15.08 secs, 9,476,121,264 bytes)
-}

--Ejercicio: Dada una cantidad c i un %x de interés anual, generar la lista
--infinita usando iterate de cantidades en que se convierte c cada año.
--Usar esa lista para calcular, dados c, %x i una cantidad objetivo o,
-- en cuantos años la cantidad será menor o igual que o
interes::Float->Float->[Float]
interes c x = iterate ((1+x)*) c
--(\k->k+(x*k/100))

numAnys::Float->Float->Float->Int
numAnys c x o = length (takeWhile (<= o) (interes c x))
--si hi poses un filter evidentment et petarà a la cara ja que no acbara mai de flirtrar la llista.