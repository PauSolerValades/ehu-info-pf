import Data.Numbers.Primes

perfectEuclidEuler = [mersenne*2^(p-1) | p <- [2..], let mersenne = 2^p-1, lucaslehmerPrime p]
    where
    s mp 1 = mod 4 mp
    s mp n = ((s mp $ n-1)^2-2) `mod` mp
    lucaslehmerPrime::Int->Bool
    lucaslehmerPrime 2 = True
    lucaslehmerPrime p = s (2^p-1) (p-1) == 0
