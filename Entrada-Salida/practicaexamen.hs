{-
evaluación no cíclica:

repeat x = x : repeat x
take 3 (repeat x)
take 3 (x:repeat x)
x:take 2 (repeat x)
x:take 2 (x:repeat x)
x:x:take 1 (repeat x)
x:x:take 1 (x:repeat x)
x:x:x:take 0
x:x:x:[]
[x,x,x]

evaulación sí cíclica

take 3 (repeat x)
take 3 (xs)
take 3 (x:xs)
x:take 2 (xs)
x:take 2 (x:xs)
x:x:take 1 (xs)
x:x:take 1 (x:xs)
x:x:x:take 0 (xs)
x:x:x:[]

-}

{-
Recoredem les funcions elementals de les iteracions

iterate f x = zs where zs = x : map f zs --es ciclica
repeat x = xs where xs = x:xs
cycle xs = xs where xs ++ xs

getLine
-}

agafaNombre::IO Int
agafaNombre =
    do
        n <- getLine
        return (read n :: Int)
