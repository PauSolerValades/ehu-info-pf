juego:: Int -> IO ()  
-- Comprueba que la clave està entre 1 y 100.
-- En caso afirmativo, inicia el juego y lo gestiona
juego clave =   
      if not (elem clave [1..100]) 
      then putStrLn "La clave debe estar entre 1 y 100"
      else  do 
            limpiarPantalla
            putStrLn  "Adivina un número entre 1 y 100"
            n <- getLine
            jugar clave (read n) --read convierte al tipo que toca. Lo ingfere haskell en lo que él cree pero como se ve en el otro fichero, le puedes forzar a intetnar castear a otro tipo

limpiarPantalla :: IO ()  
limpiarPantalla = putStr ['\n' | i<-[1..200]]

jugar :: Int -> Int -> IO ()  
-- gestiona el juego del jugador 2
jugar clave n = 
  if not (elem n [1..100]) 
  then putStrLn "Debe estar entre 1 y 100"
    else if n == clave 
         then putStrLn ("Lo adivinaste es: "++ show n)   
         else do
              n' <- pedirNuevoNum n clave 
              jugar clave n' 

pedirNuevoNum:: Int -> Int -> IO Int
-- (pedir n clave) informa al usuario sobre 
-- el número anterior y le pide uno nuevo
pedirNuevoNum n clave = 
    let 
    info True = "menor" 
    info False = "mayor"
    in  do
        putStrLn ("Mi número es " ++ 
                  info (clave<n) ++ 
                  " que " ++ show n)
        s <- getLine
        return (read s)

--Último ejercicio :( Cambiar el programa para que responda: 
-- "adivinaste que era el" ++ show n ++ "en" ++ (show k) ++ "intentos"
-- Es decir, cambiar los parametros de manera que pueda displayear los intentos.
{-
juegoIntentos:: Int -> IO ()  
-- Comprueba que la clave està entre 1 y 100.
-- En caso afirmativo, inicia el juego y lo gestiona
juegoIntentos clave =   
      if not (elem clave [1..100]) 
      then putStrLn "La clave debe estar entre 1 y 100"
      else  do 
            limpiarPantalla
            putStrLn  "Adivina un número entre 1 y 100"
            n <- getLine
            jugarIntentos clave (read n) 0 --read convierte al tipo que toca. Lo ingfere haskell en lo que él cree pero como se ve en el otro fichero, le puedes forzar a intetnar castear a otro tipo

--jugarIntentos :: Int -> Int -> Int -> IO ()  
-- gestiona el juego del jugador 2
jugarIntentos clave n k = 
  if not (elem n [1..100]) 
  then putStrLn "Debe estar entre 1 y 100"
    else if n == clave 
         then putStrLn ("Adivinaste que era el " ++ show n ++ " en " ++ (show k) ++ " intentos")   
         else do
              n' <- pedirNuevoNum n clave 
              jugarIntentos clave n' (k+1)
              
-}
