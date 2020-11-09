data ArBin a = Hoja a | Unir (ArBin a) a (ArBin a) deriving (Eq)

a1 = Nodo(Nodo Vac 3 (Nodo Vac 5 Vac)) 6 (Nodo (Nodo Vac 7 Vac) 8 Vac)

instance (Show a) => Show (ArBin a) where
    show Vac = ""
    show t = show' t 0 (maxProf t + 1)

maxProf::(Show a) => ArBin a -> Int