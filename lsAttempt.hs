--concatenates rows at end
concatRows::[String] -> String
concatRows [] = []
concatRows (x:xs) = x ++ "\n" ++ (concatRows xs)

--sets up empty lists
setUpEmpty::(Num a,Eq a) => a -> [[b]] 
setUpEmpty 0 = []
setUpEmpty n = []:(setUpEmpty (n-1))

--adds filler spaces to an item where necessary
fillUp::Int -> String -> String
fillUp 0 x = x
fillUp n x = fillUp (n-1) (x++" ")

--finds maximum length item in list
maxLength::[String] -> Int
maxLength [] = undefined
maxLength [x] = length x
maxLength (x:xs) = max (length x) (maxLength xs)

--adds filler to everything
addFiller':: Int -> [String] -> [String]
addFiller' _ [] = []
addFiller' n (x:xs) = (fillUp (n+2) x):(addFiller' n xs)

addFiller:: [String] -> [String]
addFiller xs = addFiller' (maxLength xs) xs

--figures out min no. of rows
getTotalChars:: [String] -> Int
getTotalChars [] = 0
getTotalChars (x:xs) = (length x) + (getTotalChars xs)

getNoOfRows:: [String] -> Int -> Int
getNoOfRows xs width = ((getTotalChars xs) + (2*length xs)) `mod` width

--converts to list of names
toNames:: [Entry] -> [EntryName]
toNames = map getName

--adds two lists together
addLists:: [String] -> [String] -> [String]
addLists [] [] = []
addLists [] ls = ls
addLists (x:xs) (y:ys) = (y++x): (addLists xs ys)

--small function doing most of the work
ls' n [] acc = acc
ls' n ls acc= ls' n (drop n ls) (addLists (addFiller (take n ls)) acc)

ls :: Int -> Entry -> String
ls termWidth (Dir a ys) = let Dir a ys = sortTree (Dir a ys)
			      names = toNames ys
			      rows = getNoOfRows names termWidth
			  in concat $ ls' rows names (setUpEmpty rows)

