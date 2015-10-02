Lab exercises. Unassessed but mandatory. 

You will be asked to confirm in Canvas whether you have attempted
them, and to what degree of success.

You may work in pairs if you wish.

If the lab doesn't give you enough time for this, please try to
complete as much as possible at home.

This handout turns out to be a "literate" Haskell program, that you
can run. You can tell this because it has the ".lhs" ending. The
normal ending is ".hs" when we wish to be illiterate. :-)

Please run it, and modify it as suggested.

The Haskell code is preceded by "> ". Everything else is a comment,
such as this very sentence.

Download this file from Canvas (you probably have already done that).

Open a terminal, change to the folder containing this file using the
"cd" command.

Run ghci as follows:

   $ ghci lab1-handout.lhs

Get help!

   *Main> :help

The following commands will be useful: :r :l :e :t :q
Familiarize yourself with them by reading ":help"'s answer.

Task 1.

> maxHelper n [] = n
> maxHelper n (x : xs) | n < x     = maxHelper x xs
>                      | otherwise = maxHelper n xs                     

 ANSWER 1: maxHelper:: (Ord a) => a -> [a] -> a
What is the most general type of the above function? First think, then
ask ghci with the ":t" or ":type" command.

Using the above, complete the following definition, without using
recursion:

> --maxElem :: [Integer] -> Integer
> maxElem :: (Ord a) => [a] -> a
> maxElem [] = undefined
> maxElem (x:xs) = maxHelper x xs
> maxElem2 [] = undefined
> maxElem2 [x] = x
> maxElem2 (x:xs) = max x (maxElem2 xs)

We will have trouble with the empty list: leave that case
(deliberately) undefined.

What is a more general type for maxElem?

ANSWER: maxElem :: (Ord a) => [a] -> a

Task 2. More challenging.

Think of a function String->Integer as a menu in a restaurant. There
are two things that we may want to know: the value of the most
expensive food, or the most expensive food itself. Write functions to
compute that. The list of Strings is what food is available.

> maxValue :: [String] -> (String -> Integer) -> Integer
> maxValue [] _ = undefined
> maxValue [x] f = f x
> maxValue (x:xs) f = max (f x) (maxValue xs f)

> argMax :: [String] -> (String -> Integer) -> String
> argMax [] _ = undefined
> argMax [x] _ = x
> argMax (x:xs) f = if (f x) > (f (argMax xs f)) then x else argMax xs f


Give more general types to the above two functions.

   maxValue :: (Ord b) => [a] -> (a -> b) -> b
   argMax   :: (Ord b)  => [a] -> (a -> b) -> a


Task 3. More challenging.

Here is a correct, inefficient, definition of the fibonacci function:

> fib n | n == 0    = 0
>       | n == 1    = 1
>       | otherwise = fib(n-2) + fib(n-1)

What is the most general type of this function?

ANSWER: fib :: (Eq a, Num a, Num b) => a -> b

Run it to convince yourself that it is slow. Use ":set +s" to print
run time statistics under ghci.

We try to make it faster as follows:

> fib' :: Integer -> Integer 
> fib' = fibHelper 0 1

> fibHelper :: Integer -> Integer -> Integer -> Integer
> fibHelper base1 base2 n = if n == 0 then base1 else fibHelper base2 (base1+base2) (n-1)

Challenge: fill the gap to get a fast (linear) run time. 

If you get stuck after thinking for a while, cheat using google.

If you evaluate the expression [fib' n | n <- [0..10]] at the ghci
prompt, you should get [0,1,1,2,3,5,8,13,21,34,55].

Puzzle. Haskell has lazy lists, which are potentially infinite lists.

Here is a way to get a fast program using infinite lists.

> fibs :: [Integer]
> fibs = 0 : zipWith (+) (1 : fibs) fibs

Find out about zipWith using google or our textbook.

This gives the list of Fibonacci numbers, where the nth element is
computed in linear time.

Run this, and try to understand it using google.

If you run fibs, you get an infinite computation (try). To get a
finite computation, you can do something like take 20 fibs (try this
too).

Now we can define:

> fib'' n = fibs !! n

> getAt :: (Num b, Eq b) => [a] -> b -> a
> getAt [] _ = undefined
> getAt [x] i = if  i == 0 then x else error "Not possible"
> getAt (x:xs) i = if i== 0 then x else getAt xs (i-1)
> fib''' n = getAt fibs n

Unfortunately, this can't have the type Integer->Integer! What type
does it get?

Why? Look at the definition of (!!) in the standard prelude.

You can fix this by defining your own (!!) with a more general type.

After you have tried hard, you may wish to check the sample solutions
here: http://www.cs.bham.ac.uk/~mhe/functional-programs/lab1-solution.lhs

