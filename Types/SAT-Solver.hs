-- Decidir si una proposición es satisfactible.
-- Esto es lo que hacen los famosos SAT-solvers 
-- (de un modo más eficiente)

-- El tipo Prop

infixr 3 :& 
infixr 2 :| 
infixr 1 :->, :<->
-- C, X, Not tiene màxima prioridad por que las 
-- funciones (constructores) en Haskell la tienen
data Prop =   C Bool    -- constante booleana
            | X String  -- variable proposicional
            | Not Prop         -- negación
            | Prop :& Prop     -- conjunción
            | Prop :| Prop     -- disyunción
            | Prop :-> Prop    -- implicación
            | Prop :<-> Prop   -- doble implicación
            -- deriving Show

-- Ejemplos
prop1 = X "p" :| Not (X "p") -- Tautologia
prop2 = X "p" :& Not (X "p") -- Contradicció
prop3 = X "p" :| C False :-> C False -- Satisfactible   
        -- (p | False) -> False
        -- es satisfactible, pero no tautología
prop4 = X "p" :-> C False :| X "q"
        -- p -> ( False | q)
        -- es satisfactible, pero no tautología     
prop5 = (X "p" :-> X "q") :& X "p" :->  X "q"
prop6 = prop4 :& X "r" :-> X "a" :| prop5 

-- Formalización de un argumento
{- A, B y C son sospechosos de haber robado.
Las investigaciones concluyen que 
1) Alguno de los tres es culpable
2) Si A es culpable entonces B es inocente
3) B es inocente si y solo si A y C no son los dos culpables
Entonces es imposible que B sea culpable.
Es decir, 1) :& 2) :& 3) : "B es culpable" 
forman un conjunto insatisfactible
-}

p1 = X "culpA" :| X "culpB" :| X "culpC"
p2 = X "culpA" :-> Not(X "culpB")
p3 = Not (X "culpB") :<-> Not(X "culpA" :& X "culpC")
conjetura = Not (X "culpB")                                  
argumento = p1 :& p2 :& p3 :& Not conjetura 
  -- si argumento no tiene modelo, entonces {p1,p2,p3} ==> conjetura
modelo = p1 :& p2 :& p3 :& conjetura       
  -- si es sat, hay un modelo de {p1,p2,p3,conjetura}

-- El show està fet, tot bé.
instance Show Prop where
      show p 
       = case p of
           (C b)        -> show b
           (X s)        -> s
           (Not p)      -> showP "~" [p]
           (p1 :& p2)   -> showP " & "  [p1,p2]
           (p1 :| p2)   -> showP " | "  [p1,p2]
           (p1 :-> p2)  -> showP " -> " [p1,p2]
           (p1 :<-> p2) -> showP " <-> " [p1,p2]
           where showP s [p] = if esSimple p then s ++ show p 
                               else "~(" ++ show p ++ ")"
                 showP s [p1,p2] 
                    | esSimple p1 && esSimple p2 = show p1++s++show p2
                    | esSimple p1 = show p1++" "++ s++"("++show p2++")"
                    | esSimple p2 = "("++show p1++")"++s++ " "++ show p2  
                    | otherwise   = "("++show p1++")"++s ++"("++show p2 ++")"
                 esSimple (C b) = True
                 esSimple (X s) = True
                 esSimple (Not p) = True 
                 esSimple _ = False

-- El tipo Subs
type Subs = [(String, Bool)]
-- Una sustitución asoacia a cada variable (string) un valor de verdad (Bool)
sub1 = [("p", True), ("q", False), ("a", False), ("r", True)] --ejemplo sustitución prop6
sub2 = [("culpA", True), ("culpB", False), ("culpC", True)]
--subs debbería ser una función
-- type Subs = String -> Bool
-- Ejemplos: 
--sb1 "p" = True
--sb1 "q" = False
--sb1 "r" = True
--sb1 "a" = False
-- La función evaluar (prop no es polimorfico)
evaluar:: Subs -> Prop -> Bool
evaluar _ (C b) = b
evaluar sb (X s) = apply sb s --lo pensamos como una función
                   where apply sb s = (snd.head) (dropWhile ((/=s).fst) sb)
                   
evaluar sb (Not p) = not (evaluar sb p)
evaluar sb (p1 :& p2) = evaluar sb p1 && evaluar sb p2
evaluar sb (p1 :| p2) = evaluar sb p1 || evaluar sb p2
evaluar sb (p1 :-> p2) = not (evaluar sb p1) || evaluar sb p2
--evaluar sb (p1 :<-> p2) = evaular (p1 :-> p2) && evaular (p2 :-> p1) evaluamos alguna cosa dos veces xd
evaluar sb (p1 :<-> p2) = let ep1 = evaluar sb p1
                              ep2 = evaluar sb p2
                          in (ep1 && ep2) || not(ep1 || ep2)


   
-- La lista de todas las substituciones que son posibles modelos de una proposici�n
               
-- Las funciones principales: sat, modelos, taut

lisBool::Int->[[Bool]]
lisBool 1 = [[True], [False]]
lisBool n = map ([True] ++) (ant) ++ map ([False] ++) (ant)
   where
     ant = lisBool (n-1)

--lisSubs p sea la lista de todas las posibles substituciones de las variables de p
listSubs:: Prop -> [Subs]
listSubs p = [zip listvar listbool | listbool <- (lisBool n) ]
  where 
    listvar = (quitarRep . vars) p
    n = length listvar

-- sat:: Prop -> Bool
-- (sat p) es True si y solo si p es satisfactible 
-- i.e. es cierta para al menos una substitución
                                   
-- modelos :: Prop -> [Subs]
-- (modelos p) es la lista de todas las substituciones que hacen a p cierta

-- taut:: Prop -> Bool
-- (taut p) es True si y solo si p es una tautología 
-- i.e. es cierta para cualquier substitución.

-- EJERCICIO: hacer todos los ajustes necessarios en el programa de arriba para que el tipo subs sea string -> Bool.

graphOf f  = [(e,v) | e <- [0..], v <-[f e]] --esto es el inverso del zip en la función en la que aparece.

fromGraph::Eq a => [(a,b)] -> a -> b --esta es la que queremos.
fromGraph m i = (snd . head) (filter ((==i) . fst) m)
--(filter (==i) . fst) m::[(a,b)]
--(filter (==i) . fst) m = [(i,v), ..]
-- (snd . head)  [(i,v), ..] => v