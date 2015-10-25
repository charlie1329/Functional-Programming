{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module Exercise where



-- Exercise set 2.
--
-- 30% of the exercises are intended to be rather challenging, and
-- will allow you to get a mark above 69%, in conjunction with the
-- other available exercises, so as to get a 1st class mark. To get
-- II.2, you will need to do enough hard exercises, in addition to the
-- medium and easy ones. 
--
--      >= 70% 1st
--      >= 60% II.1
--      >= 50% II.2
--      >= 40% III
--      <= 39% fail
--
-- 
-- Do as many unassessed exercises as you can, as they should make the
-- assessed exercises easier.
--
-- You are allowed to use the functions available in the standard
-- prelude (loaded by default by ghc and ghci). You should not need to
-- use other Haskell libraries from the Haskell platform available in
-- the lab, but you are allowed to use them if you wish. However, in
-- your final submission, you should not use any IO facilities, as
-- this won't compile with the marking script.

-- This exercise set revolves around a directory tree on a computer,
-- and some Unix-like functions to manipulate them.
--
-- This exercise doesn't involve reading or writing actual files from
-- disk. Instead, we represent them internally in Haskell using a
-- "data" definition, explained below.
--
-- We have these data types:
--
--     - Entry: Can be either a file or a directory (with sub-Entries)
--     - EntryName: exactly the same as a string; 
--                  represents a directory or file name
--     - Path: exactly the same as a list of strings
--     - FileProp: file properties 


data Entry = File EntryName FileProp
           | Dir EntryName [Entry]
  deriving (Show, Eq, Read)


-- The name of a file
type EntryName = String

-- A sequence of file name components.
--
-- A path is a list of strings used to navigate down to a subdirectory
-- of a given directory. We start from an Entry, and we end up with a
-- sub-Entry, provided the path is valid. See the example in the "cd"
-- exercise below.
type Path = [String]

-- FileProp describes some attributes of a file.
--
-- The components mean size, content, and creation time,
-- in that order.
data FileProp = FP Int String Int
  deriving (Show, Eq, Read)


-- Exercise, easy. Create a FileProp that describes a file with size 3,
-- content "abc", and time 4.

exampleFP :: FileProp
exampleFP = FP 3 "abc" 4

{-

Entries describe directories and files in the file system. For instance, the
following entry describes an empty directory:

    Dir "somedirname" []

The following entry describes a file in the filesystem:

    File "somefilename" (FP 4 "xyz" 3)

The following entry describes a more complicated directory.

    Dir "uni" [File "marks.txt" (FP 1036 "..." 2014),
               Directory "examSheets" [],
               File "address.txt" (FP 65 "..." 2010)
              ]

-}

-- Exercise, easy. Create three entries that correspond to the following trees:
--
-- 1.   todo.txt, size 723, time 2015, content "do fp exercises"
--
-- 2.   empty-directory
--      |
--
-- 3.   hard drive
--      |
--      |-- WINDOWS
--      |   |
--      |   |-- cmd.exe, size 1024, time 1995, content ""
--      |   |
--      |   |-- explorer.exe, size 2048, time 1995, content ""
--      |
--      |-- Documents
--      |   |
--      |   |-- User1
--      |   |   |
--      |   |   |-- recipe.doc, size 723, time 2000
--      |   |
--      |   |-- User2
--      |   |   |
--
-- You must pay attention to the order of entries in a directory.
--
-- There is a dash in the directory name of exampleEntry2.


exampleEntry1 :: Entry
exampleEntry1 = File "todo.txt" (FP 1024 "do fp exercises" 2015)
exampleEntry2 :: Entry
exampleEntry2 = Dir "empty-directory" []
exampleEntry3 :: Entry
exampleEntry3 = Dir "hard drive" [Dir "WINDOWS" [File "cmd.exe" (FP 1024 "" 1995), File  "explorer.exe" (FP 2048 "" 1995)], Dir "Documents" [Dir "User1" [File "recipe.doc" (FP 723 "" 2000)], Dir "User2" []]]

-- Exercise, unassessed. You're given a directory as a value of type
-- Entry. In this directory there is a subdirectory with name n. Find
-- (and return) this subdirectory.

cd1 :: Entry -> String -> Maybe Entry
cd1 (File _ _ ) _ = Nothing
cd1 (Dir a ys) n = case ys of
		   [] -> Nothing
		   (x:xs) -> case x of
			     (File _ _) -> cd1 (Dir a xs) n
			     (Dir x' ys') |x' == n -> Just(Dir x' ys')
					  |otherwise -> cd1 (Dir a xs) n 

-- Exercise, easy. As before, but you need to navigate not one but
-- possibly many steps down; consecutive directory names are given in
-- the list of strings.
-- 
-- Example: Given the entry in the following drawing
-- 
--     root
--     |
--     |-- dir1
--     |   |
--     |   |-- dir1a
--     |   |   |
--     |   |   |-- dir1a1
--     |   |   |
--     |   |   |-- dir1a2
--     |   |
--     |   |-- dir1b
--     |
--     |-- dir2
--     |   |
--     |   |-- dir1a
--     |   |
--     |   |-- dir1b
--     |
--     |-- file3
-- 
-- and the path ["dir1", "dir1a"], you need to return the Entry that
-- contains dir1a1 and dir1a2.
-- 
-- If there is no such entry, return Nothing. If there is such an entry,
-- return Just that entry.
-- 
-- You can assume that there will be at most one entry with the given path.

cd :: Entry -> Path -> Maybe Entry 
cd (File _ _) _ = Nothing
cd all@(Dir a ys) [] = Just(all)
cd (Dir a ys) path@(x:xs) = case ys of
			    [] -> Nothing
			    (y:ys) -> case y of
				      (File _ _) -> cd (Dir a ys) path
				      (Dir i zs)  |i == x -> cd (Dir i zs) xs
						  |otherwise -> cd (Dir a ys) path 

-- Exercise, medium. Split a string representing a path into its
-- components. The components are separated by (forward) slashes.
-- Hint: the prelude functions for lists will be helpful here, but you
-- are not required to use them.
--
-- Examples:
--
--     explode "abc/de/fghi" = ["abc", "de", "fghi"]
--     explode "abc//fghi" = ["abc", "", "fghi"]
--
-- It is a matter of convention whether we prefer
--     explode "" = [] 
-- or
--     explode "" = [""]
-- Choose your own convention. Both will be considered to be correct.


explode :: String -> Path
explode "" = []
explode (x:xs) | x == '/' = "":explode xs
	       | otherwise = case (explode xs) of
			     [] -> [[x]]
			     (y:ys) -> (x:y):ys
		

-- Exercise, easy. The "inverse" of explode: combine components with
-- slashes to a single String.
--
-- For every string s, you must have
--
--    implode (explode s) = s
--
-- You may want to use the functions "concat", "intersperse", and/or
-- "intercalate"; the latter two are from package Data.List.

implode :: Path -> String
implode [] = ""
implode [x] = x
implode (x:xs) = x++('/':implode xs)

-- Exercise, easy. Given an Entry representing a directory, print out
-- a directory listing in a format similar to "ls -l" on Unix.
--
-- The required format is as in the following example:
--
--     size: 420 time: 5 filename1
--     size: 5040 time: 200 other.txt
--     size: 30 time: 36 filename2
--
-- You need to separate every line with a newline ('\n') character,
-- and also put a newline at the end.
--
-- Keep the files in their order in the given Entry.
--
-- You do not need to convert units, just print the numbers.

lsL :: Entry -> String
lsL (Dir a ys) = case ys of
		 [] -> ""
		 (x:xs) -> case x of
			   (File name (FP size _ time)) -> "size: " ++ (show size)  ++ " time: " ++ (show time) ++ " " ++ (show name) ++ "\n" ++lsL(Dir a xs)
			   (Dir b _) -> b ++ "\n" ++ lsL (Dir a xs)   

-- Exercise, medium. List all the files in a directory tree. Sample
-- output:
--
--    root
--    |
--    |-- dir1
--    |   |
--    |   |-- dir1a
--    |   |   |
--    |   |   |-- dir1a1
--    |   |   |
--    |   |   |-- somefile
--    |   |   |
--    |   |   |-- dir1a2
--    |   |
--    |   |-- dir1b
--    |
--    |-- file2
--    |
--    |-- dir3
--    |   |
--    |   |-- dir3a
--    |   |
--    |   |-- dir3b
--
--
-- You can assume that the entry represents a directory.
--
-- Use the newline convention as given above.

dupl :: (Num a,Eq a) => a -> String -> String
dupl 0 _ = ""
dupl n xs = xs ++ dupl (n-1) xs

lsTree :: Entry -> String
--lsTree (Dir a []) = a ++ "\n|\n"--MAY NEED TO CHANGE!!!!
lsTree (Dir a ys) = let lines n (Dir a ys) = a ++  let tree2 ls = case ls of
								  	[] -> ""
								  	(x:xs) -> (dupl 2 ("\n|" ++ (dupl n "   |"))) ++"-- " ++ case x of
													(File a _) -> a ++ (tree2 xs)
													--(Dir b []) -> b ++"\n|" ++ (dupl (n+1) "   |")++(tree2 xs) MAY NEED TO CHANGE
													(Dir b zs) -> (lines (n+1) (Dir b zs)) ++ (tree2 xs)
						   in tree2 ys
		    in lines 0 (Dir a ys) ++ "\n"


-- Exercise, challenge. Make a list of all the files and directories
-- recursively in a tree (similar to "find ." in linux). If the
-- argument fullPath is False, every entry in the returned list will
-- have only the bare directory or file name. If fullPath is True,
-- every entry is the path towards that entry,
-- e.g. "root/subdir1/subdir1a/file".
--
-- The root must be the first list item. The output will be in the
-- same order as for lsTree.
--
-- For example, if d is this directory from an earlier exercise:
--
--      hard drive
--      |
--      |-- WINDOWS
--      |   |
--      |   |-- cmd.exe, size 1024, time 1995, content ""
--      |   |
--      |   |-- explorer.exe, size 2048, time 1995, content ""
--      |
--      |-- Documents
--      |   |
--      |   |-- User1
--      |   |   |
--      |   |   |-- recipe.doc, size 723, time 2000
--      |   |
--      |   |-- User2
--      |   |   |
--
-- then we have

--       listAll False d =
--                ["hard drive", "WINDOWS", "cmd.exe", "explorer.exe"
--                ,"Documents", "User1", "recipe.doc", "User2"]
-- and
--      listAll True d = 
--                ["hard drive"
--                ,"hard drive/WINDOWS"
--                ,"hard drive/WINDOWS/cmd.exe"
--                ,"hard drive/WINDOWS/explorer.exe"
--                ,"hard drive/Documents"
--                ,"hard drive/Documents/User1"
--                ,"hard drive/Documents/User1/recipe.doc",
--                ,"hard drive/Documents/User2"]

listAll :: Bool -> Entry -> [String]
listAll _ (File a _) = [a]
listAll False (Dir a ys) = a : let listAllList ls = case ls of
						    [] -> []
						    (x:xs) -> case x of
							      (File a _) -> (a : listAllList xs)
							      (Dir b zs) -> (listAll False (Dir b zs)) ++ (listAllList xs) 
				in listAllList ys

listAll True (Dir a ys) = let listAll' path (Dir a ys) = implode(path++[a]):let wList ls = case ls of
						      				             	   [] -> []
						 				                   (x:xs) -> case x of
							   				  		(File b _) -> (implode(path++[a])++"/"++b):(wList xs)
							   				  		(Dir b zs) -> (listAll' (path++[a])(Dir b zs))++(wList xs)
			  				  		    in wList ys
			  in listAll' [] (Dir a ys)
							

-- Exercise, hard. 
--
-- Given a tree, insert a given subtree in a certain position.
--
-- It does not matter how the inserted subtree is ordered with respect
-- to the other items in the directory where it is inserted. That is,
--
--     cp (Dir "root" [Dir "subdir1" [Dir "subdir1a" []]]) (["subdir1"], Dir "subdir1b" [])
--
-- may return either
--
--     Dir "root" [Dir "subdir1" [Dir "subdir1a" [], Dir "subdir1b" []]]
--
-- or
--
--     Dir "root" [Dir "subdir1" [Dir "subdir1b" [], Dir "subdir1a" []]] .
--
-- (This function is similar-ish to the Unix 'cp' utility.)


cp :: Entry -> (Path, Entry) -> Entry
cp (File _ _) _ = undefined
cp (Dir a ys) ([],e) = Dir a (e:ys)
cp (Dir a ys) ((x:xs),e) = let cp' ls = case ls of
					[] -> []
					(z:zs) -> case z of
						  (File b f) -> z:(cp' ls)
						  (Dir b xs') | b == x -> (cp z (xs,e)):zs
							      | otherwise -> z:(cp' zs)
			   in Dir a (cp' ys)

-- Exercise, medium. Given a tree and a path, remove the file or
-- directory at that path.
--
-- You can assume that there is a file or directory at that path. If
-- there are multiple files or directories with that path, you need to
-- remove all of them.
--
-- (In that the case, the tree would not be "valid" according to isValid.)
-- NEED TO CHECK WITH SOMEONE!!!
rm :: Entry -> Path -> Entry
rm (File _ _) _ = undefined
rm (Dir a ys) [] = Dir a ys --empty path so return directory
rm (Dir a ys) (x:xs) = let rm' ls = case ls of
				    [] -> []
				    (z:zs) -> case z of
					      (File b f)  | xs == [] && x == b -> rm' zs
							  | otherwise -> z:(rm' zs)

					      (Dir b xs') | b==x && xs == [] -> rm' zs
							  | b==x -> (rm z xs):zs
							  | otherwise -> z:(rm' zs)
			in Dir a (rm' ys)
					      

-- Exercise, harder. Return a tree with all the same entries, but so
-- that the entries of each (sub)directory are in sorted order.
--
-- You may use the function `sort` from the Prelude.
--
-- If there are multiple entries with the same name in a directory,
-- you may choose any order.
--CHECK SORTED IN CORRECT WAY!!!
getName::Entry -> EntryName
getName (File a _) = a
getName (Dir a ys) = a

sortWith :: (Ord b) => (a->b) -> [a] -> [a]
sortWith _ [] = []
sortWith f (x:xs) = smaller ++ [x] ++ bigger
		  where smaller = sortWith f [a | a <- xs, f a <= f x]
			bigger = sortWith f [a | a <- xs, f a > f x] 

sortTree :: Entry -> Entry
sortTree (Dir a ys) = let sortTree' ls = case ls of
				   [] -> []
				   (z:zs) -> case z of
					     (File a f) -> (File a f):sortTree' zs
					     (Dir b xs') -> (sortTree z):sortTree' zs
		    in Dir a (sortTree' (sortWith getName ys))

-- Exercise, unassessed. Change all letters to upper case in a string.
--
-- For instance,
--
--     upcaseStr "!someString123" = "!SOMESTRING123"
--
-- Hint: look at the definition of the String type in the Prelude, and think
-- about functions related to that type.
--
-- You may use the function upcaseChar, defined below.

upcaseStr :: String -> String
upcaseStr [] = []
upcaseStr (x:xs)
	  |x `elem` ['a'..'z'] = (upcaseChar x):(upcaseStr xs)
	  |otherwise = x:(upcaseStr xs)

upcaseChar :: Char -> Char
upcaseChar c =
    if ('a' <= c && c <= 'z')
    then toEnum (fromEnum c + fromEnum 'A' - fromEnum 'a')
    else c

-- Exercise, harder. Change all the file names (as above) and their
-- properties, similar to the above exercise.
--
-- From the type of modifyEntries, you can see what the input of
-- fileMap must be.

flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f a b = f b a

modifyEntries :: Entry -> ((EntryName, FileProp) -> (EntryName, FileProp)) -> Entry
modifyEntries (File a f) fileMap = let (b,c) = fileMap (a,f) in (File b c)
modifyEntries (Dir a ys) fileMap = case ys of
				   [] -> Dir a ys
				   (z:zs) -> Dir a (map (flip' modifyEntries fileMap) ys)

-- Exercise, unassessed. Create a "Fibonacci tree".
--
-- The Fibonacci tree for n=3 looks like this:
--
--     dir3
--     |   
--     |-- dir2
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |   |   
--     |   |
--     |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir1
--     |   |
--     |   |-- file, size 0, time 0, content 0
--
--
--
-- The Fibonacci tree (fibCreate 0) is a file with name "file", size and time
-- 0, and content "". For n >= 1, fibCreate n is a directory named "dir{n}",
-- containing precisely fibCreate (n-1) and fibCreate (n-2). Exception:
-- fibCreate 1 contains only fibCreate 0, and not fibCreate (-1).
--
-- (We just made up the concept of Fibonacci trees, it is not a real thing.)

fibCreate :: Int -> Entry
fibCreate 0 = File "file" (FP 0 "" 0)
fibCreate 1 = Dir "dir1" [fibCreate 0]
fibCreate n = Dir ("dir" ++ show n) ([fibCreate (n-1)]++[fibCreate (n-2)])


-- Exercise, unassessed. Make the following infinite tree:
--
--     all
--     |
--     |-- file, size 0, time 0, content 0
--     |
--     |-- dir1
--     |   |
--     |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir2
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir3
--     |   |
--     |   |-- dir2
--     |   |   |
--     |   |   |-- dir1
--     |   |   |   |
--     |   |   |   |-- file, size 0, time 0, content 0
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir4
--     |   |
--     |   (and so on)
--     |
--     | ...
--
--
-- It is to be expected that computations such as (size fibEntry) will
-- not return a result and loop for ever (or until we run out of
-- memory). But you can still e.g. "cd" into such a tree.

fibEntry :: Entry
fibEntry = Dir "all" [(fibCreate x)|x <- [0..]]

-- Exercise, unassessed. Remove from a tree all files that are larger
-- than a certain size. You should not remove any directories.
--
-- Files that are exactly that size should be kept in.

findSmallerThan :: Entry -> Int -> Entry
findSmallerThan (Dir a ys) maxSize = let check ls = case ls of
						    [] -> []
						    (z:zs) -> case z of
							      File a (FP b c d) 
									      | b > maxSize -> check zs
									      | otherwise -> (File a (FP b c d)):check zs
							      Dir b xs' -> (findSmallerThan (Dir b xs') maxSize):check zs
				     in Dir a (check ys)

-- Exercise, challenge. Remove from a tree all files that do not
-- satisfy a given predicate. You should not remove any directories.

find :: Entry -> ((EntryName, FileProp) -> Bool) -> Entry
find (Dir a ys) predicate = let check ls = case ls of
					   [] -> []
					   (z:zs) -> case z of
						     File a f
							    | predicate (a,f) -> (File a f):check zs
							    | otherwise -> check zs
						     Dir b xs' -> (find (Dir b xs') predicate):check zs
			    in Dir a (check ys)


-- Exercise, unassessed. Given a maximum file size, a file name and its file
-- properties, return whether the file is at most that size.
--
-- (This function gets a lot of information that it doesn't need; the
-- extra arguments are so that you can easily use `findSmallerThanPred
-- maxSize` as the predicate argument to `find`, in the next
-- exercise.)

findSmallerThanPred :: Int -> ((EntryName, FileProp) -> Bool)
findSmallerThanPred maxSize (filename, (FP size _ _)) = size <= maxSize

-- Exercise, unassessed. Same as findSmallerThan, but implement it again
-- using `find` and `findSmallerThanPred`.

findSmallerThan2 :: Entry -> Int -> Entry
findSmallerThan2 root maxSize = find root (findSmallerThanPred maxSize)

-- Exercise, challenge, assessed.
--
-- List all directory and file names in the current directory in a
-- table. The table can be at most termWidth cells wide. You need to
-- use as few rows as possible, while separating columns with 2
-- spaces.
--
-- (This is similar to the Unix utility 'ls'.)
-- 
-- For instance, for terminal width 80, you might have the following
-- output:
--
--     a  d  g  j                zbcdefghijklmnc  zbcdefghijklmnf  zbcdefghijklmni
--     b  e  h  zbcdefghijklmna  zbcdefghijklmnd  zbcdefghijklmng
--     c  f  i  zbcdefghijklmnb  zbcdefghijklmne  zbcdefghijklmnh
--
--
-- The ordering is alphabetical by column, and the columns should be
-- indented as above.  You can assume that the longest directory/file
-- name is at most as long as the terminal is wide.
--
-- The first argument is the terminal width:


--returns a list of sorted entry names
toSortedNames :: [Entry] -> [EntryName]
toSortedNames = map getName

--sets up n empty lists
setUp::(Num a,Eq a) => a -> [[b]]
setUp 0 = []
setUp n = []:setUp(n-1)

--adds filler to an item where necessary
fillUp:: (Num a, Eq a) => a -> String -> String
fillUp 0 str = str
fillUp n str = fillUp (n-1) (str ++ " ")

--adds filler to everything
addFiller':: Int -> [String] -> [String]
addFiller' _ [] = []
addFiller' n (x:xs) = (fillUp (n- (length x) + 2) x):(addFiller' n xs)

addFiller:: [String] -> [String]
addFiller xs = addFiller' (maxLength xs) xs

--concatenates rows at end
concatRows::[String] -> String
concatRows [] = []
concatRows (x:xs) = x ++ "\n" ++ (concatRows xs)

--adds two lists together
addLists:: [String] -> [String] -> [String]
addLists [] [] = []
addLists [] ls = ls
addLists (x:xs) (y:ys) = (y++x): (addLists xs ys)

--finds maximum length item in list
maxLength::[String] -> Int
maxLength [] = undefined
maxLength [x] = length x
maxLength (x:xs) = max (length x) (maxLength xs)

--tries ls with a given number of rows (assuming acc is set up correctly)
ls' :: Int -> Int -> [String] -> [String] -> Maybe [String]
ls' noOfRows termWidth [] acc| maxLength acc > termWidth = Nothing
			     | otherwise = Just acc
ls' noOfRows termWidth xs acc| maxLength acc > termWidth = Nothing 
			     | otherwise = ls' noOfRows termWidth (drop noOfRows xs) (addLists (addFiller (take noOfRows xs)) acc)

--attempts with all rows until finds solution
lsAttempts :: Int -> Int -> [String] -> [String]
lsAttempts noOfRows termWidth xs = case (ls' noOfRows termWidth xs (setUp noOfRows)) of
				   Nothing -> (lsAttempts (noOfRows + 1) termWidth xs)
				   Just a -> a

--the main function
ls :: Int -> Entry -> String
ls termWidth (Dir a ys) =let (Dir b xs) = sortTree (Dir a ys) in concatRows $ lsAttempts 1 termWidth (toSortedNames xs)


-- End of exercise set 2.

