{-# LANGUAGE BangPatterns #-}
data ArBin a = Vac | Nodo (ArBin a) a (ArBin a) deriving Eq

a4 = Nodo(Nodo Vac 3 (Nodo Vac 5 Vac)) 6 (Nodo (Nodo Vac 7 Vac) 8 Vac)
--Ejemplos:
a1 = Nodo (Nodo (Nodo Vac 2 Vac)
                3 
                (Nodo Vac 5 Vac)) 
          6 
          (Nodo (Nodo Vac 7 Vac) 
                8 
                (Nodo Vac 9 Vac))              
a2 = Nodo (Nodo (Nodo Vac 17 Vac) 
                 18 
                (Nodo Vac 19 Vac) ) 
           27 
          (Nodo (Nodo Vac 37 Vac) 
                100 
                (Nodo (Nodo Vac 110 Vac)
                       115 
                      (Nodo Vac 1000 Vac)))   
a3 = Nodo (Nodo (Nodo a1 10 Vac) 
                11 
               (Nodo Vac 12 Vac)) 
           14 
          (Nodo (Nodo Vac 15 Vac) 
                16 
                a2)

instance (Show a) => Show (ArBin a) where
         show Vac = ""
         show t = show' t 0 (maxLong t + 1)

{-
maxProf::(Show a) => ArBin a -> Int
maxProf Vac = 0
maxProf (Nodo ai r ad) = 
--ai := arbol izquierda
--ad := árbol derecha
-}

show'::(Show a) => ArBin a -> Int -> Int -> String

show' Vac _ _ = ""
show' (Nodo ai r ad) desde_col long_nodo 
    = dibujo_ai ++ "\n" ++ dibujo_raiz ++ dibujo_ad ++ "\n"
    where 
        dibujo_raiz = [' ' | i<-[1..desde_col]] ++ show r
        dibujo_ai = show' ai (desde_col+long_nodo) long_nodo
        dibujo_ad = show' ad (desde_col+long_nodo) long_nodo


{-
maxLong:: (Show a) => ArBin a -> Int
-- la maxima longitud (el int) de un nodo como string en el arbol dato
maxLong Vac = 0
maxLong (Nodo ai r ad) = maximum[(length.show) r, (maxLong ai), (maxLong ad)]
-}

foldArBin ::(a -> b -> b -> b) -> b -> ArBin a -> b
foldArBin f e Vac = e
foldArBin f e (Nodo ai r ad) = f r (foldArBin f e ai) (foldArBin f e ad)

maxLong:: (Show a) => ArBin a -> Int
maxLong = foldArBin (\r a b -> maximum[long r, a, b]) 0
    where 
        long = length.show

-- Definir la funci�n numVerif que, dados un predicado (sobre  
-- los elementos del �rbol) y un �rbol, devuelve el n�mero de
-- nodos que verifican el predicado. 
-- Esta funci�n debe definirse en t�rminos de foldArBin.

numVerif::(a->Bool) -> ArBin a -> Int
numVerif p = foldArBin f 0
    where f r nvi nvd = (fromEnum (p r)) + nvi + nvd
    -- | p r = 1 + nvi + nvd
    -- | otherwise = nvi + nvd


numVerif' p = foldArBin (\r vi vd -> (fromEnum (p r)) + vi + vd) 0

-- EJERCICIO PARA CASA
-- Sea el tipo 
type ArPares a = ArBin (a,a) --No es un tipo de datos algebraico nuevo, sinó que reutlitzas data ArBin para hacerlo en tuplas.
-- Definir una función que, dado un árbol del tipo ArPares a 
-- y empleando la función del ejercicio numVerif determine 
-- el número de nodos (x,y) tales que x es menor que y

xlessy arbol = numVerif (\(x,y)->x>y) arbol

ap1 = Nodo (Nodo (Nodo Vac (2,3) Vac)
                (3,3) 
                (Nodo Vac (6,5) Vac)) 
          (6,0) 
          (Nodo (Nodo Vac (7,6) Vac) 
                (8,6) 
                (Nodo Vac (5,9) Vac))              


-- Supongamos el siguiente tipo de árboles binarios con información  
-- de dos tipos en nodos internos y hojas: 

data Arbol a b  =  Hoja a | NodoT  (Arbol a b) b (Arbol a b)

-- definimos el tipo de las expresiones aritm�ticas 
-- en términos de este tipo de �rboles

type ExpArit = Arbol Int String --tipo sinónimo de Arbol a b, dopnde a es int i string es b

instance (Show a, Show b) => Show (Arbol a b) where
    show a = case a of
        (Hoja a) -> show a
        (NodoT ard e ari) -> "(" ++ show ard ++ show e ++ show ari ++ ")"
-- PREGUNTAR: COM FER UN INSTANCE SHOW DEL TYPE HEREDANT DEL GRAN.
{-
--com quasi tot el que he estat fent aquests dies això no acaba d'anar.
instance (Show a, Show b) => Show (ExpArit a b) where
    show a = case a of
        (Hoja a) -> show a
        (NodoT ard e ari) -> "(" ++ show ard ++ show e ++ show ari ++ ")"
-}
-- Ejemplos 
exp1 :: ExpArit
exp1 = Hoja 9

exp2 :: Arbol Int String
exp2 = NodoT (Hoja 3) "*" (Hoja 5)
exp3 = NodoT exp1 "-" (NodoT (Hoja 10) "+" (Hoja 6))
exp4 = NodoT exp3 "+" exp2
--exp4  representa a la expresi�n (9 - (10+6)) + (3*5)             

-- Definir una funci�n

foldArbol :: (t1 -> t2) -> (t3 -> t2 -> t2 -> t2) -> Arbol t1 t3 -> t2
foldArbol f _ (Hoja a) = f a
foldArbol f g (NodoT ai r ad) = g r (foldArbol f g ai) (foldArbol f g ad)
   
-- Definir, en función de foldArbol, funciones que permitan:
-- (a) calcular el número de operadores de una expresi�n (es lo mismo que el número de nodos internos.)

numOperadores = foldArbol (\x->0) (\_ opi opd -> opi + opd +1)
-- (b) evaluar una expresi�n
evaluar :: Arbol Int [Char] -> Int
evaluar = foldArbol id g
    where
        g op a b
            | op == "+" = a + b
            | op == "-" = a - b
            | op == "*" = a * b
            | otherwise = error("què putes has posat?")

--la mateixa però amb una lambda
evaluar' :: Arbol Int [Char] -> Int
evaluar' = foldArbol id (\op a b -> case op of
                                        "+" -> a + b
                                        "-" -> a - b
                                        "*" -> a * b
                                        otherwise -> error("què putes has posat?"))

--PREGUNTAR: PERQUE EM GENERA PROBLEMES D'INT A INTEGER, CANVIO PER INTEGER TOT I JA FUNCIONA QUE FLIPES
--Autoresposta: Quan no determines el tipus d'algunes funcions a haskell, ell mateix assumeix el tipus que li agrada.
-- En aquest case evaluar no tenia el typing, per tant assumia que volies el tipus Integer, pero quan li declares Int tot ja funcional

--PREGUNTAR RESPECTE A LA RESPOSTA: Perque numOperadores funciona tot i tenint aquest problema sense declarar res però evaluar m'envia a la merda?

--ARBOLES N-ÁRIOS

data ArbN a = NodoN a [ArbN a] deriving (Eq, Show)

--Podemos hacer el show para practicar,però quina mandra xd

arb1 = NodoN 1 [NodoN 2 [],
                NodoN 3 [NodoN 4 [],
                         NodoN 5 [NodoN 6 [NodoN 7 []]]],
                NodoN 8 [NodoN 9 [NodoN 10 []]]]

arb2 = NodoN 1 []

foldt :: (a -> [b] -> b) -> ArbN a -> b
foldt f (NodoN r ts) = f r (map (foldt f) ts)

-- Ejercicios:
-- 1. numNodos: calcule el número de nodos
numNodos = foldt (\_ ts -> 1 + (foldr (+) 0 ts))
numNodos' = foldt (\_ ts -> 1 + sum ts)

-- 2. Función que devuelve la rama más larga como una lista
longestBranch = foldt f
    where
        f r [] = [r]
        f r zs = r: masLarga zs
        masLarga = foldr1 (\xs ys -> if length xs > length ys then xs else ys)
--En aquest m'hi he acostat perooooo... no l'he tret. He perdut molt temps entenent l'enunciat
        

--EJERCICIOS PARA CASA:
-- 1. Función que calcula la profundidad.
-- 2. Función que devuelde la lista de todos1 los nodos en preorden
profunditat = foldt f
    where
        f _ [] = 1
        f _ ts = maximum ts +1

{-
preorden = foldt f
    where
        f r [] = [r]
        f r zs = if null zs then [r] else let NodoN n xs = head zs in [r] ++ [n]
        masLarga = foldr1 (\xs ys -> if length xs > length ys then xs else ys)
        -}

preordre :: [ArbN a] -> [a]
preordre (NodoN r t:ts) = if null ts then [r] else r:preordre t --si ts es llista buida, t es l'últim element de la llista
 
{-
preorden = foldt f
    where
        f r [] = [r]
        f r zs = r:newList zs
        newList (z:zs) = if null zs then [out z] else out z:newList zs
        out (NodoN n _) = n
-}

preorden' (NodoN r ts) = if null ts then [r] else r:newList ts
    where
        newList (z:zs) = if null zs then [out z] else out z:newList zs
        out (NodoN n _) = n


fun (NodoN n ns) = if null ns then [out (head ns)] else n:fun n (tail ns)

out (NodoN n _) = n
