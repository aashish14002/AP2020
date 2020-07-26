
{-- 99 Problems in Haskell
--- Problems 1-10
--}

-- Error Constants
emptyListError = "Empty List!"
notEnoughelementsError = "Not Enough Elements!"
invalidIndexError = "Invalid index"


{-- Problem 1 Find the last element of a list.
--- myLast [1,2,3,4]
--- 4
--}

myLast :: [a] -> a
myLast [] = error emptyListError
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' [] = error emptyListError
myLast' x = x !! (length x -1) 

myLast'' :: [a] -> a
myLast'' = foldl1 (\_ x -> x )

myLast''' :: [a] -> a
myLast''' = foldr1 (\_ acc -> acc )

myLastEl :: [a] -> a
myLastEl xs = fst . (!! 0) . dropWhile (\(a,b) -> b<(length xs)) . zip xs $ [1..]


{-- Problem 2 Find the last but one element of a list.
--- myButLast [1,2,3,4]
--- 3
--}

myButLast :: [a] -> a
myButLast [] = error emptyListError
myButLast [_] = error notEnoughelementsError
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' [] = error emptyListError
myButLast' [_] = error notEnoughelementsError
myButLast' x = x !! (length x -2) 


{-- Problem 3  Find the K'th element of a list. 
--- elementAt [1,2,3] 2
--- 2
--}

elementAt :: (Ord n, Num n) => [a] -> n -> a
elementAt [] _ = error emptyListError
elementAt (x:xs) n
    | n<=0 = error invalidIndexError
    | n==1 = x
    | otherwise = elementAt xs (n-1)

zipp3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zipp3 [] _ _ = []
zipp3 _ [] _ = []
zipp3 _ _ [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zipp3 xs ys zs

elementAt' :: [a] -> Int -> a
elementAt' xs n = foldl (\acc (n, (a, b)) -> if (b > n) then acc else a ) (head xs) l
    where l = zip (replicate (length xs) n ) . zip xs $ [1..(length xs)]

elementAt'' :: [a] -> Int -> a
elementAt'' xs n = foldl (\acc (n, a, b) -> if (b > n) then acc else a ) (head xs) l
    where l = zipp3 (replicate (length xs) n )  xs  [1..(length xs)]


{-- Problem 4  Find the number of elements of a list.
--- myLength [123, 456, 789]
--- 3
--}

myLength :: [a] -> Int
myLength [] = 0
myLength [_] = 1
myLength (_:xs) = 1 + myLength xs

myLength' xs = foldl (\acc x -> acc) 0 xs

{-- Problem 5  Reverse a list.
--- myReverse [1,2,3,4]
--- [4,3,2,1]
--}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: (Num a) => [a] -> [a]
myReverse' xs = foldl (\acc x -> x:acc) [] xs



{-- Problem 6  Find out whether a list is a palindrome. 
--- A palindrome can be read forward or backward; e.g. (x a m a x).
--- isPalindrome [1,2,3]
--- False
--}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs)
    | x == (myLast xs) = True && isPalindrome (init xs)
    | otherwise = False 



{-- Problem 7  Flatten a nested list structure.
--- Transform a list, possibly holding lists as elements into a
--- `flat' list by replacing each list with its elements (recursively).
--- data NestedList a = Elem a | List [NestedList a]
--- flatten (Elem 5)
--- [5]
--- flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
--- [1,2,3,4,5]
--}

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a] 
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten (x) ++ flatten (List xs)


{-- Problem 8  Eliminate consecutive duplicates of list elements.
--- If a list contains repeated elements they should be replaced with a
--- single copy of the element. The order of the elements should not be changed.
--- compress "aaaabccaadeeee"
--- "abcade"
--}
 
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:(y:xs))
    | x == y =  compress(y:xs)
    | otherwise = [x] ++ compress (y:xs)


{-- Problem 9  Pack consecutive duplicates of list elements into sublists. 
--- If a list contains repeated elements they should be placed in separate sublists.
--- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
---      'a', 'd', 'e', 'e', 'e', 'e']
--- ["aaaa","b","cc","aa","d","eeee"]
--}
 
-- pack :: (Eq a) => [a] -> [a]
-- pack [] = []
-- pack [a] = [a]
-- pack (x:(y:xs))
--     | x == y =  pack(y:xs)
--     | otherwise = [x] ++ pack (y:xs)