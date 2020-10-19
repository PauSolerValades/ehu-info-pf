import Data.List
import Data.Function
import Data.Char

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

--EJERCICIO PROPUESTO: define letterFreq, que devuelve una lista de tuplas (letra, num) que cuente cuantas veces aparece cada char en una frase
listConst :: [Char] -> [[(Char, Int)]]
listConst xs = map (:[]) (sort (zip xs [1,1.. length xs -1]))

letterFreq :: [Char] -> [(Char, Int)]
letterFreq str = tupleAdd(concatLists(listConst (str)))

letterFreqComp str = (tupleAdd . (concatLists . listConst)) str

tupleAdd:: [[(Char, Int)]]->[(Char,Int)]
tupleAdd [[]] = []
tupleAdd (x:xs)
   | xs == [] = (x!!0):(tupleAdd [[]])
   | otherwise = (foldl (\(x,y) (z,t)->(z,y+t)) (' ',0) x):(tupleAdd xs)

concatLists:: (Eq a) => [[a]] -> [[a]]
concatLists [] = []
concatLists (x:xs)
    | xs == []               = x:concatLists []
    | (head xs)!!0 == x!!0   = concatLists((x++head xs):(tail xs))
    | (head xs)!!0 /= x!!0   = x:(concatLists xs)
    | otherwise      = concatLists []
    

letterFreqClasse:: String -> [(Char, Int)]
letterFreqClasse str = (sumar.agrupar) (zip str [1 | i <- [0..]])
                       where agrupar::[(Char,Int)]->[(Char,Int)]
                             xs = (filter (\(c,x) -> (pred c)).sort)
                             pred c = not (isSpace c && isPunctuation c)
                             agrupar = (xs.map (\(c,x) -> (toLower c, x)))
                             sumar = foldr op []
                             op :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
                             op (c, x) [] = (c,x):[] --[(c,x)] també val
                             op (c, x) ((d, y):resto) = 
                                 if (c==d) 
                                 then (c,x+y):resto 
                                 else (c,x):(((d, y):resto))
--op = \(c,x) ps -> if c == (fst.head) ps 
--then (c,c+(snd.head)ps):(tail ps)
--else (c,x):ps
                                 
-- EJERCICIO PROPUESTO: que solo saque letras, ni signos de exclamacion ni espacios
-- EJERCICIO PROPUESTO 2: cuente mayúsculas y minúsculas iguales.

-- EJERCICIO PERMUTACIONES DE LA LISTA 1: hacer todas las permutaciones de una lista dada.

perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms p = [x:xs | x <- p, xs <- perms (delete x p)]
          where
          delete x xs = (takeWhile (/=x) xs) ++ tail (dropWhile (/=x) xs)         
-- Ejercicio: Usar span para recorrer xs una sóla vez  
         
-- EJERCICIO: usar span para recorrer xs una sola vez

--9
--set +s dice el tiempo que tarda en computar

triadsxLTyLTz n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y+1..n], x^2 + y^2 == z^2]
triadsThr n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y+1..n], mod x 2 /= 1 || mod y 2 /=1, x^2 + y^2 == z^2]

--EJERCICIO PROPUESTO: Un número es perfecto si es igual a la suma de todos sus divisores excepto el mismo. Dissenyar una funcion que obtenga, dado un n, todos los numeros perfectos menores que este.

perfect n = [x | x <- [1..n], sum(divisors x) == x]
    where
    divisors::Int->[Int]
    divisors n = [y | y <- [1..(n-1)], n `rem` y == 0]

perfectEuclidEuler = [mersenne*2^(p-1) | p <- [2..], let mersenne = 2^p-1, arePrime p]
    where
    arePrime n = [x | x <- [2..n], n `rem` x == 0] == [n]



--p is the index of the mersenne number 2^p-1
--lucaslehmerPrime p mersenne =




--EJERCICIO PROPUESTO: Conjetura de Collatz. Mirar que es. Definir una función que obtenga todas las cadenas de collatz de losgitud menor o igual a un n dado que hay entre los números de inicio x e y. Oju que és difícil, anar amb compte i no fa falta acabarla


myLenght::[a]->Int
myLenght l = aux(0 l)
    where
        aux::Int->[a]->Int
        aux i x:xs
            | xs == [] = i+1
            | otherwise aux i+1 xs
