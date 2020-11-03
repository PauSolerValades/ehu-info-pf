data Expr a =  N a  
              | Expr a :+: Expr a  
              | Expr a :-: Expr a  
              | Expr a :*: Expr a  
              | Expr a :/: Expr a 
              deriving (Eq, Show)



ejem1 = N 5 :+: N 3 :+: N 4
ejem2 = N 5 :+: N 8 :*: N 2
-- por las expresiones: N 5 :+: (N8 :*: N 2) = 21
-- NO (N 5 :+: N 8) :*: N 2 que seria 26

--evaluar :: Fractional a => Expr a -> a
evaluar (N x) = x
evaluar (e1 :+: e2) = evaluar e1 + evaluar e2
evaluar (e1 :-: e2) = evaluar e1 - evaluar e2
evaluar (e1 :*: e2) = evaluar e1 * evaluar e2
evaluar (e1 :/: e2) = evaluar e1 / evaluar e2

--Ejercicios:
-- 1. Hacer (Exp a) instancia de la clase Show (mostrar como queramos)
-- 2. Hacer (Exp a) instancia de la clase Eq usando evaluar (tendremos que quitar el deriving)
-- 3. Hacer (Exp a) instancia de la clase Num

-- Per fer això necessitaré entendre tota aquesta merda, nais. Em poso a la biblio de la resi i a tope

{-̣
SatSolver = primer problema demostrado NPCompleto.
Haremos uno aquí, que no será demasiado refinado. Los industriales estan muy
optimizados.
-}
