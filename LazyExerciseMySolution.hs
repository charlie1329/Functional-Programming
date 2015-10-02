sieve :: [[Int]] 
sieve = sievefrom 2 (repeat []) 

sievefrom :: Int -> [[Int]] -> [[Int]] 
sievefrom n ([]  : xs) = []: sievefrom (n+1) (cross n xs)
sievefrom n (x : xs) =  x : sievefrom (n+1) xs

cross :: Int -> [[Int]] -> [[Int]]
cross n xs = cross' (n-1) xs
  where 
    cross' 0 (x : xs) = (n:x) : cross' (n-1) xs
    cross' i (x : xs) = x     : cross' (i-1) xs

primeFactorsTo n = take n sieve

-- SUCCESSFUL SOLUTION!!!!
