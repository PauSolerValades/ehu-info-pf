--Longitud lista
myFunList::(Eq a) => [a]->Int
myFunList [] = 0
myFunList (_:xs) = myFunList(xs) + 1

--Ejercicio 2a examen
longestNestedListaFun::[[a]]->Int --el tipo debe ser [[a]]->[a]
longestNestedListaFun (x:[]) = length x
longestNestedListaFun (x:xs) = if length x < length (head xs) 
                               then longestNestedListaFun xs 
                               else longestNestedListaFun (x:(tail xs))
-- Definir una función que calcule la lista más larga de una lista de listas, no su longitud

--Ejercicio 4 examen
grayFun::Int->[String]
grayFun 1 = ["0","1"]
grayFun n = map ("0"++) (prev_gray) ++ map ("1"++) (reverse(prev_gray))
    where
    prev_gray = grayFun (n-1)
--PERFECTO
    
--Cadenas de collatz    
collatz_chains n = [reverse(collatz_chain [x]) | x<-[1..n]]

collatz_chain::[Int]->[Int]
collatz_chain xs = if (head xs)==1 then xs else collatz_chain(collatz_next((head xs)):xs)

collatz_next::Int->Int
collatz_next n
    | mod n 2 == 0  = div n 2
    | mod n 2 == 1  = 3*n + 1

-------------- Una solución mas fácil
collatz :: Int -> Int
collatz 1 = 1
collatz n = if (even n) then (n `div` 2) else (3 * n + 1)

collatzChain :: Int -> [Int]
collatzChain n =
  if n == 1
    then [1]
    else [n] ++ collatzChain (collatz n)
{-
*Main> collatzChain 500
[500,250,125,376,188,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

*Main> collatzChain 12133
[12133,36400,18200,9100,4550,2275,6826,3413,10240,5120,2560,1280,640,320,160,80,40,20,10,5,16,8,4,2,1]

*Main> collatzChain 15133
[15133,45400,22700,11350,5675,17026,8513,25540,12770,6385,19156,9578,4789,14368,7184,3592,1796,898,449,1348,674,337,1012,506,253,760,380,190,95,286,143,430,215,646,323,970,485,1456,728,364,182,91,274,137,412,206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]
-}
    
-- Números perfectos

perfect n = [x | x <- [1..n], sum(divisors x) == x]
    where
    isqrt :: Int -> Int
    firstHalf::Int->[Int]
    secondHalf::Int->[Int]
    divisors::Int->[Int]
    isqrt = floor . sqrt . fromIntegral
    divisors n = [1] ++ firstHalf n ++ secondHalf n
    firstHalf n = [y | y <- [2..isqrt n], n `rem` y == 0]
    secondHalf n = [(div n y) | y <- (firstHalf n)]



--Por la evlauación perezosa, isPrime para cuando encuentra un FALSE, ya que toda la lista va a dar false
-- Incluyo la division del uno para que siempre de True el primero

perfectEuclidEuler :: [Int]
perfectEuclidEuler = [mersenne*2^(p-1) | p <- [2..], let mersenne = 2^p-1, isPrime mersenne]
    where
    isqrt :: Int -> Int
    isqrt = floor . sqrt . fromIntegral
    isPrime p = and([p `mod` x /= 0 | x <- [2..isqrt p]])

perfectEuclidEulerPro = [mersenne*2^(p-1) | p <- [2..], let mersenne = 2^p-1, lucaslehmerPrime p]
    where
    s mp 1 = mod 4 mp
    s mp n = mod ((s mp $ n-1)^2-2) mp
    lucaslehmerPrime::Int->Bool
    lucaslehmerPrime 2 = True
    lucaslehmerPrime p = s (2^p-1) (p-1) == 0

-- muy bien tus investigaciones para hacerlo eficiente, pero pasa algo raro, mira:
{-
*Main> take 10 perfectEuclidEulerPro 
[6,28,496,8128,33550336,8589869056,137438691328,2305843008139952128,2658455991569831744654692615953842176,191561942608236107294793378084303638130997321548169216]

*Main> take 10 perfectEuclidEuler
[6,28,496,8128,2096128,33550336,8589869056,137438691328,35184367894528,144115187807420416]

2096128 no es perecto
-}



    


