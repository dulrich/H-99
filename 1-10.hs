-- problem 1
myLast (x:[]) = x
myLast (x:xs) = myLast xs


-- problem 2
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs


-- problem 3
elementAt [] _ = error "list too short" 
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)


-- problem 4
myLength [] = 0
myLength (x:[]) = 1
myLength (x:xs) = (myLength xs) + 1


-- problem 5
myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]


-- problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome $ init xs)


-- problem 7 ??? had to look this one up, need to work on algebraic data types/syntax
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
-- main = print (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))


-- problem 8
compress [] = []
compress [x] = [x]
compress (x:y:ys) = if (x == y)
	then (compress ([y] ++ ys))
	else ([x] ++ compress ([y] ++ ys))


-- problem 9
prePack :: (Eq a) => ([a],[a]) -> [[a]]
prePack ((x:xs),[y]) = if (x == y)
	then [[x]++xs++[y]]
	else [[x]++xs,[y]]
prePack ((x:xs),(y:ys)) = if (x == y)
	then (prePack ([x]++xs++[y],ys))
	else ([[x]++xs] ++ prePack ([y],ys))

pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:xs) = prePack ([x],xs)

-- main = print (pack "aaaabccaadeeee")


--problem 10
encode :: (Eq a) => [a] -> [(Int,a)]
encode x = map (\y -> (length y,head y)) $ pack x

main = print (encode "aaaabccaadeeee")
