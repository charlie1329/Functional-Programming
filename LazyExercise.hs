sieve :: [Bool] -- Change this type to [[Int]]
sieve = sievefrom 2 (repeat True) -- Change this too appropriately.

sievefrom :: Int -> [Bool] -> [Bool] -- Change the type appropriately.
sievefrom n (True  : xs) = True  : sievefrom (n+1) (cross n xs) -- What else needs to be changed?
sievefrom n (False : xs) = False : sievefrom (n+1) xs

cross :: Int -> [Bool] -> [Bool] -- From now on you are on your own.
cross n xs = cross' (n-1) xs
  where 
    cross' 0 (x : xs) = False : cross' (n-1) xs
    cross' i (x : xs) = x     : cross' (i-1) xs

primes :: [Int]  
primes = primesfrom 2 sieve

primesfrom :: Int -> [Bool] -> [Int]
primesfrom n (True   : xs) = n : primesfrom (n+1) xs
primesfrom n (False  : xs) =     primesfrom (n+1) xs
