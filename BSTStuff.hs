data BT a = Empty | Fork a (BT a) (BT a) deriving(Show, Read, Eq, Ord)

--creating some auxillary functions to make life easier

leaf :: a -> BT a
leaf x = Fork x Empty Empty

newTree :: () -> BT a
newTree () = Empty

root :: BT a -> a
root Empty = error "You can't get the root of an empty tree"
root (Fork a _ _) = a

left :: BT a -> BT a
left Empty = error "You can't get the left subtree of an empty tree"
left (Fork _ l _) = l

right :: BT a -> BT a
right Empty = error "You can't get the right subtree of an empty tree"
right (Fork _ _ r) = r

isEmpty :: BT a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- now some useful functions for a BT

--finds the height of the binary tree recursively
height :: (Num b,Ord b) => BT a -> b
height Empty = 0
height (Fork _ l r) = 1 + max (height l) (height r)

--finds the size of the binary tree recursively
size :: (Num b) => BT a -> b
size Empty = 0
size (Fork _ l r) = 1 + (size l) + (size r)

--finds the sum of the binary tree recursively if type of tree is numerical
treeSum :: (Num a) => BT a -> a
treeSum Empty = 0
treeSum (Fork a l r) = a + (treeSum l) + (treeSum r)

--mirrors the whole tree (doesn't hold property of BST)
mirror :: BT a -> BT a
mirror Empty = Empty
mirror (Fork a l r) = Fork a (mirror r) (mirror l) 

--inserts new item into a BST
insert :: (Ord a) => a -> BT a -> BT a
insert x Empty = leaf x
insert x (Fork a l r)
	| x < a = Fork a (insert x l) r
	| x > a = Fork a l (insert x r)
	| otherwise = error "Not allowed to have duplicates in a BST"

--finds smallest item in a binary search tree
smallestNode :: (Ord a) => BT a -> a
smallestNode Empty = error "No nodes in an empty tree"
smallestNode (Fork a Empty _) = a
smallestNode (Fork _ l _) = smallestNode l

--deletes an item from a bst (if present)
delete :: (Ord a) => a -> BT a -> BT a
delete _ Empty = Empty
delete x (Fork a l r)
	|a < x = Fork a (delete x l) r
	|a > x = Fork a l (delete x r)
	|isEmpty l = r
	|isEmpty r = l
	|otherwise = let k = smallestNode r in
			Fork k l (delete k r)

--traverses the tree using in-order traversal (only really suitable for BST)
inOrder :: BT a -> [a]
inOrder Empty = []
inOrder (Fork a l r) = (inOrder l) ++ a:(inOrder r)

--traverses the tree using pre-order traversal (only really suitable for BST)
preOrder :: BT a -> [a]
preOrder Empty = []
preOrder (Fork a l r) = a:(preOrder l) ++ (preOrder r)

--traverses the tree using post-order traversal (only really suitable for BST)
postOrder :: BT a -> [a]
postOrder Empty = []
postOrder (Fork a l r) = (postOrder l) ++ (postOrder r) ++ [a]

--the following two functions determine whether a binary tree is a valid binary search tree
isBST' :: (Ord a) => [a] -> Bool
isBST' [] = True
isBST' [_] = True
isBST' (x:y:xs) 
	|x < y = isBST'(y:xs)
	|otherwise = False

isBST :: (Ord a) => BT a -> Bool
isBST tree = isBST' (inOrder tree)

--the following function searches for an item in a BST and returns a Bool value appropriately
findInTree :: (Eq a, Ord a) => a -> BT a -> Bool
findInTree _ Empty = False
findInTree x (Fork a l r)
	|x == a = True
	|x > a = findInTree x r
	|x < a = findInTree x l

--the following function is the quicksort algorithm
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smaller ++ [x] ++ larger
	where smaller = [y |y <- xs, y <= x]
	      larger = [y | y <- xs, y > x]

--the following takes a list of items and creates a BST out of them
listToTree :: (Ord a) => [a] -> BT a
listToTree [] = Empty
listToTree (x:xs)
	|findInTree x tree = tree
	|otherwise = insert x tree
	where tree = listToTree xs 

--testing the implementation
test = newTree ()
test2 = insert 7 test
test3 = insert 9 test2
test4 = insert 8 test3
test5 = delete 7 test4

