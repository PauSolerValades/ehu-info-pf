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

perimetro r = 2*r*pi
area r = r*r*pi

--EJERCICIO4

--longitud x = if div x 10 >= 0 then longitud (div x 10) else 

agregar::Int->Int
agregar x y = if (isDigit 'x' && isDigit 'y') then x*10^(numberLength y) + y else putStrLn "Algo ha anat malment :("

--EJERCICIO5
sumcuad x y = x*x + y*y + 2*x*y

