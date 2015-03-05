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


-- problem 9 (pack "aaaabccaadeeee")
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


--problem 10 (encode "aaaabccaadeeee")
encode :: (Eq a) => [a] -> [(Int,a)]
encode x = map (\y -> (length y,head y)) $ pack x


-- problem 11 (encodeModified "aaaabccaadeeee")
data RunLength a = Single a | Multiple Int a
	deriving (Show)
encodeModified :: (Eq a) => [a] -> [RunLength a]
encodeModified x = map (\y -> if (length y == 1)
	then (Single (head y))
	else (Multiple (length y) (head y))) $ pack x


-- problem 12 (decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'])
decodeModified :: [RunLength a] -> [a]
decodeModified [Single x] = [x]
decodeModified [Multiple n x] = replicate n x
decodeModified ((Single x):xs) = [x] ++ (decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs)


-- problem 13
-- encodeDirect :: (Eq a) => [a] -> [RunLength a]
-- encodeDirect x = map (\y -> if (



-- problem 14
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs


-- problem 15 (repli "abc" 3)
repli [] _ = []
repli [x] 1 = [x]
repli [x] n = [x] ++ repli [x] (n - 1)
repli (x:xs) n = (repli [x] n) ++ (repli xs n)


-- problem 16
dropEvery' [] _ _ = []
dropEvery' [x] 1 _ = []
dropEvery' [x] n _ = [x]
dropEvery' (x:xs) 1 c = (dropEvery' xs c c)
dropEvery' (x:xs) n c = (dropEvery' [x] n c) ++ (dropEvery' xs (n - 1) c)

dropEvery [] _ = []
dropEvery x n = dropEvery' x n n


-- problem 17 (split "abcdefghik" 3)
splitShift :: ([a],[a]) -> Int -> ([a],[a])
splitShift (x,[]) _ = (x,[])
splitShift (x,y) 0 = (x,y)
splitShift (x,(y:ys)) n = splitShift (x ++ [y],ys) (n - 1)

split [] _ = ([],[])
split [x] 0 = ([],[x])
split [x] _ = ([x],[])
split x n = splitShift ([],x) n


-- problem 18 (slice "abcdefghik" 3 5)
slice [] _ _ = []
slice [x] 1 _ = [x]
slice [x] _ _ = []
slice (x:xs) 1 1 = [x]
slice (x:xs) i k = (slice [x] i k) ++ (slice xs (max 1 (i - 1)) (max 1 (k - 1)))


-- problem 19 (rotate "abcdefghik" (-3))
rotate [] _ = []
rotate [x] _ = [x]
rotate x 0 = x
rotate (x:xs) n = if (n > 0)
	then rotate (xs ++ [x]) (n -1)
	else rotate ([last xs] ++ [x] ++ init xs) (n + 1)


-- problem 20
removeAt [] _ = []
removeAt [x] 1 = []
removeAt [x] _ = [x]
removeAt (x:xs) n = (removeAt [x] n) ++ (removeAt xs (n - 1))

main = print (removeAt "abcdefghik" 2)



