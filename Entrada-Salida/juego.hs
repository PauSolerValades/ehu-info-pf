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
            jugar clave (read n)

limpiarPantalla :: IO ()  
limpiarPantalla = putStr ['\n' | i<-[1..24]]

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
        putStrLn ("Mi n�mero es " ++ 
                  info (clave<n) ++ 
                  " que " ++ show n)
        s <- getLine
        return (read s)