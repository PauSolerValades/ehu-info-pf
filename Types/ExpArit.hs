infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Expr a =  N a
              | Expr a :+: Expr a  
              | Expr a :-: Expr a  
              | Expr a :*: Expr a  
              | Expr a :/: Expr a 



ejem1 = N 5 :+: N 3 :+: N 4
ejem2 = N 5 :+: N 8 :*: N 2
-- por las expresiones: N 5 :+: (N8 :*: N 2) = 21
-- NO (N 5 :+: N 8) :*: N 2 que seria 26

--evaluar :: Fractional a => Expr a -> a
--evaluar :: Fractional a => Expr a -> a
evaluar (N x) = x
evaluar (e1 :+: e2) = evaluar e1 + evaluar e2
evaluar (e1 :-: e2) = evaluar e1 - evaluar e2
evaluar (e1 :*: e2) = evaluar e1 * evaluar e2
evaluar (e1 :/: e2) = evaluar e1 / evaluar e2

--Ejercicios:
-- 1. Hacer (Exp a) instancia de la clase Show (mostrar como queramos) DONE
-- 2. Hacer (Exp a) instancia de la clase Eq usando evaluar (tendremos que quitar el deriving)
-- 3. Hacer (Exp a) instancia de la clase Num
-- Per fer això necessitaré entendre tota aquesta merda, nais. Em poso a la biblio de la resi i a tope

instance Show a => Show (Expr a) where
    show (N a) = show a
    show (N a :+: N b) = "(" ++ show (N a) ++ "+" ++ show (N b) ++ ")"
    show (N a :-: N b) = "(" ++ show (N a) ++ "-" ++ show (N b) ++ ")"
    show (N a :*: N b) = "(" ++ show (N a) ++ "*" ++ show (N b) ++ ")"
    show (N a :/: N b) = "(" ++ show (N a) ++ "/" ++ show (N b) ++ ")"
    show (a :+: N c) = show(a) ++ "+" ++ show (N c)
    show (a :-: N c) = show(a) ++ "-" ++ show (N c)
    show (a :*: N c) = show(a) ++ "*" ++ show (N c)
    show (a :/: N c) = show(a) ++ "/" ++ show (N c)
    show (N a :+: b) = show(N a) ++ "+" ++ show b
    show (N a :-: b) = show(N a) ++ "-" ++ show b
    show (N a :*: b) = show(N a) ++ "*" ++ show b
    show (N a :/: b) = show(N a) ++ "/" ++ show b

{-
Preguntar perque instance Show Expr where
    show a = case a of ........ li falten coses / no funciona
-}

instance (Eq a, Fractional a) => Eq (Expr a) where
    (N a) == (N b) = a==b
    a == b = evaluar a == evaluar b

--No et mentié, això no m'acaba de sortir...

instance Num a => Num (Expr a) where
    N a + N b = N (a+b)
    N a - N b = N (a-b)
    N a * N b = N (a*b)
    negate (N a) = N (-a)
    abs (N a) = N a
    signum (N a) = N a
    fromInteger (a) = (N a)

