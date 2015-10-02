mem :: (Eq a) => a -> [a] -> Bool
mem _ [] = False
mem i (x:xs) = x == i || mem i xs

prefix :: (Eq a) => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

prefixes' :: [a] -> [a] -> [[a]]
prefixes' [] p = [p]
prefixes' (x:xs) p = p:prefixes' xs (p ++ [x])

prefixes :: [a] -> [[a]]
prefixes lst = prefixes' lst []

every' :: (Integral b) => b -> b -> [a] -> [a]
every' _ _ [] = []
every' n 1 (x:xs) = x:every' n n xs
every' n i (x:xs) = every' n (i-1) xs 

every :: (Integral b) => b -> [a] -> [a]
every n xs = every' n n xs

