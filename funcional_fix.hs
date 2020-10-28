--Longitud lista
myFunList::(Eq a) => [a]->Int
myFunList [] = 0
myFunList (_:xs) = myFunList(xs) + 1

--Ejercicio 2a examen
longestNestedListaFun::[[a]]->Int
longestNestedListaFun (x:[]) = length x
longestNestedListaFun (x:xs) = if length x < length (head xs) then longestNestedListaFun xs else longestNestedListaFun (x:(tail xs))

--Ejercicio 4 examen
grayFun::Int->[String]
grayFun 1 = ["0","1"]
grayFun n = map ("0"++) (prev_gray) ++ map ("1"++) (reverse(prev_gray))
    where
    prev_gray = grayFun (n-1)
    
--Cadenas de collatz    
collatz_chains n = [reverse(collatz_chain [x]) | x<-[1..n]]

collatz_chain::[Int]->[Int]
collatz_chain xs = if (head xs)==1 then xs else collatz_chain(collatz_next((head xs)):xs)

collatz_next::Int->Int
collatz_next n
    | mod n 2 == 0  = div n 2
    | mod n 2 == 1  = 3*n + 1
    
-- Números perfectos

perfect n = [x | x <- [1..n], sum(divisors x) == x]
    where
    divisors::Int->[Int]
    divisors n = [y | y <- [1..(n-1)], n `rem` y == 0]

perfectEuclidEuler = [mersenne*2^(p-1) | p <- [2..], let mersenne = 2^p-1, arePrime p]
    where
    arePrime n = [x | x <- [2..n], n `rem` x == 0] == [n]

{-
--necesita include de la libreria primes, yo me la bajé y ejecuto este fichero dentro de la misma carpeta
perfectEuclidEuler n = [mersenne*2^(p-1) | p <- [2..n], let mersenne = 2^p-1, mersenne <= n && isPrime mersenne]
-}

perfectEuclidEulerPro = [mersenne*2^(p-1) | p <- [2..], let mersenne = 2^p-1, lucaslehmerPrime p]
    where
    s mp 1 = mod 4 mp
    s mp n = mod ((s mp $ n-1)^2-2) mp
    lucaslehmerPrime::Int->Bool
    lucaslehmerPrime 2 = True
    lucaslehmerPrime p = s (2^p-1) (p-1) == 0
    


