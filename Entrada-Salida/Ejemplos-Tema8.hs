import Data.Char     -- para poder usar la funcion toUpper

echo :: IO ()
echo = getChar >>= (\c -> putChar c) --la c se está instanciando con getChar, el >>= significa operación sequencial

echo' :: IO ()
echo' = getChar >>=  (\c -> putChar '\n' >>= (\_ -> putChar c))
             
echoR :: IO Char
echoR = getChar >>=  (\c -> (putChar c >>=  \_ -> return c ))

--cuando lleva un igual, >>= debe usar el resultado que le viene, en cambio >> no lo necesita. las siguietnes líneas de código son equivalentes a lo escrito ya,
{-         
echo' :: IO () 
echo' = getChar >>= \c -> putChar '\n' >> putChar c

echoR :: IO Char 
echoR = getChar >>= \c -> putChar c >> return c 
-}

--------------------------------------------------------------------
-- EJERCICIO: Programa que lee un caràcter y lo imprime en mayúscula 
--------------------------------------------------------------------

inMayus::IO ()
inMayus = getChar >>= (\c -> putChar (toUpper c)) >> putChar '\n'

inMayus'::IO ()
inMayus' = getChar >>=  (\c -> putChar '\n' >>= (\_ -> putChar (toUpper c))) >> putChar '\n'

inMayusDo = --equivalent a inMayus', pero aquí s'enten
        do
        c <- getChar
        putChar '\n'
        putChar (toUpper c)
        putChar '\n'
        -- c' <- getChar --esto peta porque qué vas a hacer con la última cosa si no la recoge nadie


miGetLine:: IO String  
-- lee el string de entrada hasta '\n'. es una recursion con un parámetro implicito, el input
miGetLine =
    do  
    c <- getChar
    if c=='\n' then return ""
               else do  
                    cs <- miGetLine
                    return (c:cs)

pedirLeer:: IO String
--pide y produce como resultado el string que se teclee
pedirLeer =do
           putStrLn "Teclee password + Enter"
           getLine

coincide :: String -> IO Bool
--(coincide pass) lee un string y decide si es
--es idèntico a pass
coincide pass = do
                tecleado <-pedirLeer
                return (tecleado == pass)

pedirComprobar:: String -> IO ()
--(pedirComprobar p) pide al usuario un caracter,
---lo lee y escribe un mensaje indicando si coincide
--con p
pedirComprobar p = do
                   sonIg <-coincide p
                   putChar '\n'
                   if sonIg then putStrLn "Correcto"
                            else putStrLn "Incorrecto"

-----------------------------------------------------------------

-- Programa que lee un entero

leerEnt :: IO Int
leerEnt =  do
        e <- getLine
        return (read e)

-- Programa que lee una lista de enteros (escrita como lista):

leerLisEnt :: IO [Int]
leerLisEnt = do   
               lin <- getLine
               return (read lin)

-- Programa que lee una lista de enteros, aplica la función parámetro e imprime la lista resultante:

mapLisEnt :: Show a => (Int -> a) -> IO( )
mapLisEnt f  = do  
                  lis <- leerLisEnt
                  print (map f lis)

---------------------------------------------------------------------------
-- EJERCICIO: Programa que lee enteros (uno en cada línea) y los devuelve en una lista.
---------------------------------------------------------------------------

addIntList :: IO [Int]
addIntList = do
        x <- getLine
        if x == "" then return [] --oleee la llista buidaaaa
                   else do
                           xs <- addIntList
                           return((read x :: Int):xs) -- el tipatje de l'int és redundant amb tipatge de la funció.
        

----------------------------------------------------------------------------
-- EJERCICIO: Programa que lee enteros (uno en cada l�nea) e imprime la suma de todos ellos.
---------------------------------------------------------------------------

--la cosa està en que, si has de fer servir comandes d'entrada i sortida, ha d'estar dins d'un do. En cas que no hi sigui, doncs no anirà!
addIntListSum :: IO ()
addIntListSum = do
        l <- addIntList
        print (sum l)

--se puede poner un let para no calcular dos veces.
addIntListSumRet :: IO Int
addIntListSum = do
        l <- addIntList
        print (sum l)
        return (sumList) --para que devuelva un entero como resultado pues tienes que poner el return, sinó el tipo es IO ().

-- Programas que tratan con ficheros de entrada/salida
-- "C:/Users/..../Desktop/entrada.txt"

convertirMay =
        do
        putStrLn "Teclee el nombre|path del fichero dato"
        ficheroIn <-getLine
        s <-readFile ficheroIn
        putStrLn "Teclee el nombre|path del ficher resultado"
        ficheroOut <-getLine
        writeFile ficheroOut (map toUpper s)
        putStrLn (ficheroIn ++ " ha sido convertido en "++ ficheroOut)

seguidoMay =
        do
        putStrLn "Teclee el nombre del fichero"
        fichero <-getLine
        s <-readFile fichero
        appendFile fichero ("\n\n\t --EL RESULTADO ES: --\n\n"
                             ++ (map toUpper s))
        putStrLn ("RESULTADO EN " ++ fichero)
