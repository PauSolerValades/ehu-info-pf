--obrir les commandes: ghci

import Data.Char


--EJERCICIO1
--primera dona un error per les funcions. Correcció:
--(3==-(-3)) && True
--'a' no es comparable a True
--si x no està definida, el compliador no sap què es
--x=True
--x==True || x== False 
--Lultima no té problema

--EJERCICIO2:parla de les funcions, com sapliquen com les matemàtiques. més enllà. La primera es resol per 1 i la segona per qualsevol enter.

--EJERCICIO3
perimetro::Float->Float
area::Float->Float
perimetro2::Float->Float
area2::Float->Float

perimetro r = 2*r*pi
perimetro2 = (2*).(pi*)
area r = r*r*pi
area2 = (* pi).(^2) --això és amb composició de funcions que es molt guai i ja. S'evalua com l'entrada composada amb ^2 que es r² composat amb pi per tant es pi*r² y olee

--EJERCICIO4

--longitud x = if div x 10 >= 0 then longitud (div x 10) else 

--agregar::Int->Int->Int
--agregar x y = if (isDigit 'x' && isDigit 'y') then x*10 + y else putStrLn "Algo ha anat malment :("
agregar::Int->Int->Int
agregar x y = if (y>=0 && y<=9) then x*10+y else error "no es un digit :("

numDigits :: Integer -> Integer
numDigits n = toInteger (round (logBase 10 (fromIntegral n)))

agregar_int x y = x*(10^(numDigits y))+ y

--EJERCICIO5
sumcuad::(Int, Int)->Int
sumcuad (x, y) = x^2 + y^2

sort:: Int->Int->Int->(Int, Int)
sort x y z
        | x >= y && y>=z    = (x,y)
        | x >= y && y<=z    = (x,z)
        | otherwise         = (y,z)

final x y z = sort. sumcuad

dosMay:: Int->Int->Int->(Int, Int)
dosMay x y z
        | x >= y && y>=z    = (x,y)
        | x >= y && y<=z    = (x,z)
        | otherwise         = (y,z)

sumcuad2::Int->Int->Int->Int
sumcuad2 x y z = a^2 + b^2 where (a,b) = dosMay x y z

--variante: que sume el cuadrado del mayor de los con el cubo del mas pequeño de los cos que quedan.

--EJERCICIO6
myDivMod::(Int, Int)->(Int, Int)
myDivMod (x,y) = (div x y, mod x y)

--EJERCICIO7
--resulta que si poses fromEnum 'n' et converteix n no la variable que tu li pases xd
sigLetra::Char->Char
sigLetra n
        | n == 'Z'          = 'A'
        | n == 'z'          = 'a'
        | not (isAlpha n)   = error "n no es una letra"
        | otherwise         = toEnum (fromEnum n +1)

--EJERCICIO8 pacasa
--digitoVal::Char->Int

--EJERCICIO9
prod::Int->Int->Int
-- assumimos n<=m
prod n m
    | n == m    = n
    | n < m     = n * prod (n+1) m
    | otherwise = error "Has posat els nombres al revés"




