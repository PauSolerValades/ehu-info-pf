raices::(Float, Float, Float) -> (Float, Float)
raices (a,b,c)
       | a==0       = (-c/b,0.0)
       | dif < 0    = error "arrles complexes"
       | otherwise  = ((-b-s)/d, (-b+s)/d)
                    where   dif = b*b -4*a*c
                            s = sqrt dif
                            d = 2*a

positLista::[Int]->[Int]
positLista [] = []
positLista (x:s) = if x > 0 then x:(positLista s) else positLista s

--Ejercicio 1
quitaUltimo [] = []
quitaUltimo (x:s) = if s /= [] then x:(quitaUltimo s) else s 

quitaUno x (y:ys) = if x == y then ys else y : quitaUno x ys

--version no curryficada de quitaUno
quitaUno':: (Eq a) => (a, [a]) -> [a]
quitaUno' (_,[]) = []
quitaUno' (x, (y:ys)) = if x == y then ys else y : quitaUno' (x, ys)
-- amb -t (funcio) torna el tipus de la funció

--Ejercicio 2
isIn::Int->[Int]->Bool
isIn a (x:s)
        | x == a    = True
        | s == []   = False
        | otherwise = isIn a s

quitaRep [] = []
quitaRep (x:s) = if isIn x s then quitaRep s else x : quitaRep s

quitarRep' [] = []
quitarRep' (x:xs) = x : quitarRep' (quitarTodos x xs)
                  where
                  quitarTodos _ [] = []
                  quitarTodos z (y:ys) = if z == y then quitarTodos z ys else y : quitarTodos z ys

--per ajuntar una element a una llista, poses x:s, com després del else. El primer igual de quitaRep [] = [] serveix per parar la recursió, és a dir, que fa el que sembla  que fa de primeres.

--Ejercicio 3
dif:: (Eq a) => [a] -> [a] -> [a]
dif xs [] = xs
dif xs (y:ys) = dif (quitaUno y xs) ys

--Ejercicio 4
perm:: (Eq a) => [a] -> [a] -> Bool
perm xs ys = (dif xs ys == []) && (dif ys xs == [])

--Ejercico 5
--Si compares llistes necessites el Eq(a)
sonpermde1:: (Eq a) => [[a]] -> [[a]]
sonpermde1 [] = [] --això és la llista que no conté cap llista.
sonpermde1 (x:xs) = x:sonperm x xs
                    where 
                    sonperm:: (Eq a) => [a] -> [[a]] -> [[a]]
                    sonperm y [] = []
                    sonperm y (z:zs) = if perm z y then z: sonperm y zs else sonperm y zs

sonpermde2::(Eq a) => [[a]] -> [[a]]
sonpermde2 [] = []
sonpermde2 (x:xs) = x:filter (perm x) xs

--Ejercicio 6
timesTen [] = []
timesTen (x:xs)
        | xs /= []      = x*10^(length xs):timesTen xs
        | xs == []      = x:xs

aDecimal'::[Int]->Int
aDecimal' xs = foldr (+) 0 (timesTen xs)

aDecmial:: [Int]->Int
aDecimal xs = foldl g 0 xs
                where 
                g x y = 10*x + y


aDigit 0 = []
aDigit x = aDigit (x `div` 10) ++ [x `mod` 10]



