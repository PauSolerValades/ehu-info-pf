import Data.Char     -- para poder usar la funcion toUpper

echo :: IO () 
echo = getChar >>= (\c -> putChar c)

--echo' :: IO () 
--echo' = getChar >>=  (\c -> putChar '\n' >>= (\_ -> putChar c))
             
--echoR :: IO Char 
--echoR = getChar >>=  (\c -> (putChar c >>=  \_ -> return c ))

         
echo' :: IO () 
echo' = getChar >>= \c -> putChar '\n' >> putChar c

echoR :: IO Char 
echoR = getChar >>= \c -> putChar c >> return c 


--------------------------------------------------------------------
-- EJERCICIO: Programa que lee un car�cter y lo imprime en may�scula 
--------------------------------------------------------------------

prog0 :: IO ()
prog0 = getChar >>= (\c -> putChar (toUpper c))

prog1 :: IO ()
prog1 = getChar >>= (\c -> putChar (toUpper c)) >> putChar '\n'

prog2 :: IO ()
prog2 = do
         c <- getChar
         putChar (toUpper c)
         putChar '\n'
         --c' <- getChar

prog3 :: IO Char        
prog3 = getChar >>= (\c -> (putChar (toUpper c) 
                             >> putChar '\n' >> return c))
         
-------------------------------------------------
miGetLine:: IO String  
-- lee el string de entrada hasta '\n'
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
--es id�ntico a pass
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

-- Programa que lee una lista de enteros, aplica la funci�n par�metro e imprime la lista resultante:

mapLisEnt :: Show a => (Int -> a) -> IO( )
mapLisEnt f  = do  
                  lis <- leerLisEnt
                  print (map f lis)

---------------------------------------------------------------------------
-- EJERCICIO: Programa que lee enteros (uno en cada l�nea) 
--            y los devuelve en una lista.
---------------------------------------------------------------------------

--leerListEntLineas :: IO [Int]
leerListEntLineas = do
                    line <- getLine
                    if line == "" then return []
                    else do
                          restLines <- leerListEntLineas
                          --return (((read line)::Int):restLines)
                          return ((read line):restLines)
                          
--------------------------------------------------------------------
-- EJERCICIO: Programa que lee enteros (uno en cada l�nea) 
--            e imprime la suma de todos ellos.
--------------------------------------------------------------------
leeYsuma1 :: IO ()
leeYsuma1 = do 
            lisEnt <- leerListEntLineas
            print (sum lisEnt)

leeYsuma2 :: IO Int
leeYsuma2 = do 
            lisEnt <- leerListEntLineas
            print (sum lisEnt) 
            return (sum lisEnt)            
            
leeYsuma3 :: IO Int          
leeYsuma3 = do lisEnt <- leerListEntLineas
               let s = sum lisEnt 
                   in do print s
                         return s      
              






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
        --putStrLn s --això va doncs
        -- putStrLn (map (\_->' ') s) això sí va
        -- putStrLn (map (\_->" ") s) no va per algun motiu
        appendFile fichero ("\n\n\t --EL RESULTADO ES: --\n\n"
                             ++ (map toUpper s))
        putStrLn ("RESULTADO EN " ++ fichero)
