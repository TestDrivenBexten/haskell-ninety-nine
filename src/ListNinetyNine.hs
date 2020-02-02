module ListNinetyNine
    ( lastInList
    ) where

lastInList :: [a] -> a
lastInList [] = error "Empty list"
lastInList [x] = x
lastInList (_:xs) = lastInList xs
