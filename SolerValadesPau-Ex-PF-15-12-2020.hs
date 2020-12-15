{-
EJERCICIO 1: Definir, por ajuste de patrones, una función sumPares que sume todos los pares 
             de elementos consecutivos de una lista de enteros dada. Por ejemplo:
				*Main> sumPares [4,6,2,8,3,10]
				[10,8,10,11,13]
-}


sumPares (x:[]) = []
sumPares (x:xs) = (x+(head xs)):sumPares (xs)

{-
EJERCICIO 2: Definir, usando la función predefinida zipWith, una función sumParesZ que haga
             lo mismo que la función sumPares del ejercicio 1. 
-}


sumParesZ::[Integer]->[Integer]
sumParesZ [] = []
sumParesZ (x:xs) = zipWith (+) (x:xs) xs


{-
El triángulo de Pascal es un lista infinita de listas (finitas) de números tal que
  1.- la primera lista está formada por el número 1;
  2.- cada una de las restantes listas se construyen sumando los números adyacentes
      de la lista anterior y añadiendo un 1 al principio y al final. 
      
Una vista, como triángulo, de las 9 primeras listas, con cada lista en una fila es:
          1
         1 1
        1 2 1
      1  3 3  1
     1 4  6  4 1
    1 5 10 10 5 1
   1 6 15 20 15 6 1
  1 7 21 35 35 21 7 1
 1 8 28 56 70 56 28 8 1
-}

{-
EJERCICIO 3: Definir, usando la función sumParesZ (o sumPares), una función sigFila que 
             dada una lista del triangulo de Pascal, calcule la siguiente lista en dicho triángulo. 
             Por ejemplo:
				*Main> sigFila [1,6,15,20,15,6,1]
				[1,7,21,35,35,21,7,1]
-}

sigFila::[Integer] -> [Integer]
sigFila [] = [1]
sigFila xs = 1:sumParesZ xs ++ [1]

{-
EJERCICIO 4: Definir, usando la función sigFila, una función recursiva pascal1 que construya la lista infinita
             de listas que define el triángulo de pascal. Por ejemplo:
				*Main> take 9 pascal1
				[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1],
				[1,7,21,35,35,21,7,1],[1,8,28,56,70,56,28,8,1]]
-}

pascal1:: [[Integer]]
pascal1 = iterate (sigFila) []

{-
EJERCICIO 5: Definir, usando iterate y zipWith, una función pascal2 que haga exactamente lo mismo que
             la funcion pascal1 del ejercicio anterior.
-}

pascal2::[[Integer]]
pascal2 = iterate (\(x:xs) -> 1:(zipWith (+) (x:xs) xs)++[1]) [1]

--usando sumParesZ tiene más sentido, pero yo sigo el enunciado


pascal2'::[[Integer]]
pascal2' = iterate (\xs -> 1:sumParesZ xs ++[1]) [1]

{-
EJERCICIO 6: Definir, usando pascal2 (o pascal1) y take, una acción de entrada/salida que pida al usuario 
             el número de filas e imprima el triangulo de Pascal de dicho número de filas
             del siguiente modo:
				*Main> main
				Teclea un número de filas
				9
				Triángulo de 9 filas:
				1
				1 1
				1 2 1
				1 3 3 1
				1 4 6 4 1
				1 5 10 10 5 1
				1 6 15 20 15 6 1
				1 7 21 35 35 21 7 1
				1 8 28 56 70 56 28 8 1

            Indicación: 
            Las siguientes funciones son predefinidas en Haskell:

			unlines :: [String] -> String
			unlines [] = []
			unlines (x:xs) = x ++ '\n' : unlines xs

			unwords :: [String] -> String
			unwords []     =  ""
			unwords (w:ws) = w ++ go ws
							 where
							 go []     = ""
							 go (v:vs) = ' ' : (v ++ go vs)
-}


main = do
         putStrLn "Teclea un número de filas: "
         n <- getLine
         putStrLn ("Triángulo de " ++ n ++ " filas: ")
         putStrLn ("\n" ++ (superPrint (read n::Int)))

--sé que probablemente no se tuviera que hacer esto pero bueno, funciona

pascaln n = map toString (take n pascal2)

superPrint n = unlines (map unwords (pascaln n))

toString::[Integer] -> [String]
toString xss = map (map intToChar) (map (aDigitos) xss)

intToChar::Integer->Char
intToChar n
    | n == 0    = '0'
    | n == 1    = '1'
    | n == 2    = '2'
    | n == 3    = '3'
    | n == 4    = '4'
    | n == 5    = '5'
    | n == 6    = '6'
    | n == 7    = '7'
    | n == 8    = '8'
    | n == 9    = '9'


--aDigitos::Integer->[Integers]
aDigitos n
        | 0 <= n && n <= 9 = [n] --caso simple
        | otherwise = aDigitos (n `div` 10) ++ [n `mod` 10]

{-
EJERCICIO 7: Tal y como imprimimos el triángulo de Pascal en el ejercicio anterior, 
             tenemos que el elemento de la fila 7 y columna 4 es el 20 y que no hay elemento
             en la fila 4 y columna 5. 
             Definir una función pascalFC que dada una fila f y una columna c, calcule el elemento 
             del triángulo de Pascal que ocupa la posición (f,c) en el triángulo, si este existe,
             en caso de no exitir la función debe devolver error. 
             Por ejemplo:
				*Main> pascalFC 2 2
				1

				*Main> pascalFC 7 4
				20

				*Main> pascalFC 4 5
				*** Exception: La columna no puede ser mayor que la fila

                *Main> pascalFC 9 6
                56
            Indicación: Puede definirse recursivamente sin usar las funciones previas que calculan 
            el triángulo completo, basta con hacer las sumas adecuadas.
-}

--no me da tiempo de hacerlo más bonito lo siento
--pascalFC::Integer->Integer->Integer
pascalFC x y = if x < y then error "la fila no puede ser mayor que la columna" else calcPosition x y
    where
        calcPosition x y = (take x (pascal2) !! (x-1)) !! (y-1)
            

{-
EJERCICIO 8: Usando el tipo Maybe, definir una acción IO que pida al usuario la fila y la columna 
             e imprima el elemento de esa fila y columna o el mensaje de error. Por ejemplo:

                *Main> mainFC
				Teclea la fila
				7
				Teclea la columna
				4
				El elemento en (7,4) es 20

				*Main> mainFC
				Teclea la fila
				4
				Teclea la columna
				5
				No existe (4,5)
-}

{-

-- algo así es la idea

mainFC = do
          putStrLn "Teclea la Fila"
          f <- getLine
          putStrLn "Teclea la Columna"
          c <- getLine
          if (read f :: Int) < (read c :: Int) then maybe ("no existe" ++ show f ++ show c)
          else putStrLn "El elemento en" ++ f ++ n ++ " es " ++ show (pascalFC (read f :: Int) (read c :: Int)) 
-}
