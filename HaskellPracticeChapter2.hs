doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleUs' x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x <= 100 then 2*x else x
doubleSmallNumber' x = (if x <= 100 then 2*x else x) + 1
remainderThree =  [x | x <- [50..100], x `mod` 7 == 3]
bangBoom xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, myOdd x]
myOdd x = x `mod` 2 /= 0
notThese = [x | x <- [10..20], x/=13, x/= 15, x/= 19]
myProducts xs ys = [x*y | x <- xs, y <- ys]
myLength xs = sum[1 | _<- xs]
onlyUpperCase xs = [x | x <- xs, x `elem` ['A'..'Z']]
removeAllOdd xs = [[x | x <- y , not(odd x)]|y <- xs]
triangles = [(a,b,c)| c <-[1..10], b <- [1..c], a<-[1..b], a^2 + b^2 == c^2, a + b + c == 24]

