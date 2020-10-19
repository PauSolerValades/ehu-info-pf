import Data.Numbers.Primes

perfectEuclidEuler n = [mersenne*2^(p-1) | p <- [2..n], let mersenne = 2^p-1, mersenne <= n && isPrime mersenne]




--p is the index of the mersenne number 2^p-1
--lucaslehmerPrime p mersenne =
