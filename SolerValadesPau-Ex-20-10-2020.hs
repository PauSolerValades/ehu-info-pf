{-
Pau Soler Valadés, no tengo especialidad que estoy de sicue :)
-}
import Data.List

--Ejercicio 1a

mapIfa::(a->Bool)->(a->a)->[a]->[a]
mapIfa p f [] = []
mapIfa p f (x:xs) = 
    if p x then (f(x)):(mapIfa p f xs) 
    else x:(mapIfa p f xs)

--Ejercicio 1b

mapIfb::(a->Bool)->(a->a)->[a]->[a]
mapIfb p f xs = [y | x<-xs, let y = (g p f x)]
    where
    g::(a->Bool)->(a->a)->a->a
    g p f x = if p x then f x else x

--Ejercicio 2a

longestNestedLista::[[a]]->Int
longestNestedLista (x:xs) = aux 0 (x:xs)
    where
    aux::Int->[[a]]->Int
    aux i [] = i
    aux i (x:xs) = if (length x >= i) then aux (length x) xs else aux i xs
    
longestNestedListaFun::[[a]]->Int
longestNestedListaFun (x:[]) = length x
longestNestedListaFun (x:xs) = if length x < length (head xs) then longestNestedListaFun xs else longestNestedListaFun (x:(tail xs))

--Ejercicio 2b

longestNestedListb::[[a]]->Int
longestNestedListb xs = length(foldr1 bigger xs)
    where
    bigger::[a]->[a]->[a]
    bigger xs ys
        | length xs >= length ys   = xs
        | otherwise                = ys

--Ejercicio 3

gray::Int->[String]
gray 1 = ["0","1"] --definimos el inicio, desde donde construiremos todas las otras
gray n = aux 1 (gray 1) 
    where
    aux::Int->[String]->[String]
    aux i xs
        | i == n-1    = next_gray xs
        | otherwise   = aux (i+1) (next_gray xs)
    next_gray::[String]->[String]
    next_gray xs = map ("0"++) xs ++ map ("1"++) (reverse xs)

--Ejercicio 4a

esCreciente::(Ord a) => [a]->Bool --hemos tenido que añadir ord a para poder usar todos los tipos comparables
esCreciente [] = True
esCreciente [x] = True
esCreciente xs = and (zipWith (<) (xs) (tail xs))

--Ejercicio 4b

esOrdenada::(a->a->Bool) -> [a] -> Bool
esOrdenada p xs = and (zipWith p (xs) (tail xs))
