--type Fecha = (Int,Int,Int) -- tipo sinónimo

newtype Fecha = New(Int,Int,Int) --tipo nuevo. Lo puedes llamar de qualquier manera, no solo New. Tiene que pasar en mayuscula.
                deriving Eq --, Ord No da el orden en fechas como lo quiero

instance Ord Fecha where
    New(d,m,a) <= New(d',m',a') = (a,m,d) <= (a',m',d')
  
--Ejercicio: mostrar la fecha usando una funcion auxiliar showmes
--que muestre Enero, Febrero, Marzo,  etc

instance Show Fecha where
    show (New(d,m,a)) = 
        show d ++ "-" ++ showmes m ++ "-" ++ show a
        where showmes m = case m of
                              1 -> "Enero"
                              2-> "Febrero"
                              3-> "Marzo"
                              4-> "Abril"
                              5-> "Mayo"
                              6-> "Junio"
                              7-> "Julio"
                              8-> "Agosto"
                              9-> "Septiembre"
                              10-> "Octubre"
                              11-> "Noviembre"
                              12-> "Diciembre"
                        

-- Ejercicio: Utilizar una función de tipo fold para definir una 
-- función que dada una lista de Fechas obtenga la fecha más temprana. 
fechaMasTemp :: [Fecha] -> Fecha
fechaMasTemp [] = error "no hay ninguna fecha"
--fechaMasTemp [f] = f --sobra
fechaMasTemp fs = foldr1 min fs

data Racional = Int :/ Int -- Data es un TIPO ALGEBRAICO. Para cosas de algebra.

-- Ejercicio: Definir Racional como una instancia de la clase Eq,
-- tal que por ejemplo 1/2 == 2/4 == 3/6,  etc.

-- Si poses que la teva classe Racional deriving Eq, 1/2 != 2/4. En canvi, si tu li defineixes el == amb Instance Eq, pots posar el que vulguis
instance Eq Racional where 
  (x:/y) == (x':/y') = x*y' == y*x'
  
-- Ejercicio: Racional instancia de la clase Ord 
-- tal que p.e. 2/5 < 2/4
  
instance Ord Racional where 
  (x:/y) <= (x':/y') = x*y' <= y*x'

-- Ejercicio: Hacer una instancia de Show para el tipo Racional
              -- que muestre 1:/2 como 1/2
              -- que muestre x/x como 1
              -- que muestre 2/4 como 1/2
instance Show Racional where
    show (x :/ y) = if den == 1 then show num else (show num) ++ "/" ++ (show den)
           where
           z = gcd x y
           num = div x z
           den = div y z
       
-- Si fas una funció sense una instancia show, haskell no et pot treure per pantalla cap resultat.
sumaRac::Racional -> Racional ->Racional
sumaRac (a:/b) (c:/d) = ((a*d + c*b) :/ (b*d))

maxListRac:: [Racional] -> Racional
maxListRac = foldr1 max
