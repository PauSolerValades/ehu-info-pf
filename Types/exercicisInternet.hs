--6.1
newtype Coords a = Coords (a, a, a)

type C = Coords Float

distance::C -> C -> Float
distance (Coords (x,y,z)) (Coords (a,b,c)) = norma2 pq
    where
        pq = Coords (a-x, b-y, c-z)
        norma2 (Coords (x,y,z))= sqrt(x*x +y*y +z*z)

--6.2
data Action = Stop | NoChange | Start | SlowDown | PrepareToStart deriving (Show, Eq, Enum)
data TrafficLigth = Red | Yellow | Green deriving (Eq)

drive::TrafficLigth->TrafficLigth->Action
drive s1 s2 = case (s1,s2) of
    (Yellow, Red) -> Stop
    (Red, Green) -> Start
    (Red, Red) -> PrepareToStart
    (Green, Yellow) -> SlowDown
    (s1, s2) -> if s1==s2 then NoChange else error "Vaya fumada de semàfor xD"

--6.3

data Bochvar = TRUE | FALSE | MEANINGLESS

and::Bochvar -> Bochvar -> Bochvar
and TRUE TRUE = TRUE
and _ MEANINGLESS = MEANINGLESS
and MEANINGLESS _ = MEANINGLESS
and _ _ = FALSE

or TRUE TRUE = TRUE
or FALSE FALSE = FALSE
-- etc.....

data Queque a = Nil | Elem a (Queque a)

l1 = Nil
l2 = Elem 1 l1
l3 = Elem 2 (Elem 4 l2)

instance Eq a => Eq (Queque a) where
    Nil == Nil = True
    Nil == Elem _ _ = False
    Elem a q1 == Elem b q2 = a==b && q1 == q2

instance (Show a, Eq a) => Show (Queque a) where
    show l = "[" ++ show' l
        where
            show':: (Show a, Eq a) => Queque a -> String
            show' Nil = "]"
            show' (Elem a b) = if b == Nil then show a ++ "]" else show a ++ "," ++ show' b

{-
instance Enum a => Enum (Queque a) where
    succ Nil = error  "Final de la llista"
    succ (Elem _ q) = q
    prev (Elem a (Elem b q)) = 
-}
isEmpty :: Eq a => Queque a -> Bool
isEmpty q = q == Nil
topElem :: Queque a -> a
topElem (Elem a _) = a
insert :: a -> Queque a -> Queque a
insert = Elem
create :: Queque a
create = Nil