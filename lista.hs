import Data.List
import Data.Function
--3.
--elem et diu si x es a xs. Passan-li la llista estem creant la llista de booleans que tenen cada element de xs a la llista ys.
--and d'una llista de booleans et retorna la porta lògica and de tots els elements de la llista (si tot and = true, sinó dóna false)

mismosElem:: (Eq a) => [a] -> [a] -> Bool --si se tiene que comparar para algo se debwe insertar (Eq a)
mismosElem xs ys = and [elem x ys | x <- xs] && and [elem y xs | y <- ys]

--4.

--el operador xs !! x da la posicion xs[x]
--posiciones :: (Eq a) a -> [a] -> [Int]
posiciones x xs =  zip xs [0..(length xs -1)]

posiciones' x xs = [p | (e,p) <- zip xs [0..], e==x]
posiciones'' x xs = [p | (e,p) <- zip xs [0..], e==x]

--5.
posimpar :: [a]->[a]
posimpar xs = map fst (filter (odd.snd) (zip xs [0..]))

posimpar' xs = [xs !! i | i <- [1,3.. length xs -1]] -- això és una passada tu

--6.

igual xs = and [f == s | (f,s) <- zip xs (tail xs)]
igual' xs = and [f == s | (f,s) <- zip xs [head xs, head xs .. length xs -1]]

--frequencias de letras.
happyFreq :: [(Char, Int)]
happyFreq = [(x,c) | x<- ['a'..'z'], let c = (length.filter (== x)) "happy", c>0]

-- zip xs [1,1.. length xs -1]
--Agrupar [(p,1)(p,1),(y,1)...]
-- Sumar [(p,3),(y,1)]

letterFreq :: [Char] -> [(Char, Int)]
letterFreq xs = sort (zip xs [1,1.. length xs -1])
