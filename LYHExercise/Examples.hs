{-- Learn You a Haskell 
--- Problems 1-10
--}

-- Error Constants
emptyListError = "Empty List!"


head' :: [a] -> a
head' [] = error emptyListError
head' (x:_ )= x
