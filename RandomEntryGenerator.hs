
--this code should generate a reasonably random entry tree
--I am not making any claims about how good a 'random' tree it is producing though!
--Created By Charlie Street

--copied from exercise sheet
import System.Random

data Entry = File EntryName FileProp
           | Dir EntryName [Entry]
  deriving (Show, Eq, Read)

type EntryName = String

type Path = [String]

data FileProp = FP Int String Int
  deriving (Show, Eq, Read)
--end type constructors

randomInts :: [Int]
randomInts = randomRs (minBound,maxBound) (mkStdGen seed)
		where seed = 42

noOfEntries::Int -> [Int]
noOfEntries n = take n randomInts

createTree::Int -> [Int] -> Entry
createTree limit [] = File "BaseFile" (FP 0 "" 0)
createTree limit (x:xs) = Dir ("Dir"++show x) (generateList limit xs)

generateList::Int -> [Int] -> [Entry] 
generateList limit ls = case ls of
		  [] -> []
		  (x:xs) 
			 | x < 0 -> (File ("File" ++ show x) (FP 0 "" 0)):(generateList limit xs) -- <0 should be midpoint for all integers
			 | otherwise -> (createTree limit (take n ls)):(generateList limit (drop n ls)) -- will create the tree formed by directory and add to rest of list
					where n = (x `mod` limit)+ 1--don't want 0 taken as this could end up infinite which we don't want in this case

getRandomEntryTree::Int -> Entry
getRandomEntryTree n = createTree n (noOfEntries n)--this should generate a (reasonably random) entry



