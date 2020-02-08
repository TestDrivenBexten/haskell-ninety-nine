module ListNinetyNine
    ( lastInList,
      penultimateInList,
      kthInList,
      numElements,
      reverseList,
      isPalindrome,
      trimEnds,
      flattenList,
      Tree(Node,Branch),
      compress,
      pack,
      encode,
      CountStatus(Single,Multiple),
      encodeModified,
      decodeModified,
      encodeDirect
    ) where

lastInList :: [a] -> a
lastInList [] = error "Empty list"
lastInList [x] = x
lastInList (_:xs) = lastInList xs

penultimateInList :: [a] -> a
penultimateInList [] = error "Empty list"
penultimateInList [x] = error "List requires two or more elements"
penultimateInList [x,y] = x
penultimateInList (_:xs) = penultimateInList xs

kthInList :: [a] -> Int -> a
kthInList [] x = error "Empty list"
kthInList xs x = xs !! (x - 1)

numElements :: [a] -> Int
numElements [] = 0
numElements [x] = 1
numElements (_:xs) = 1 + numElements xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList [x,y] = [y,x]
reverseList (x:xs) = (reverseList xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (xs) = (head xs == lastInList xs) && isPalindrome (trimEnds xs)

trimEnds :: [a] -> [a]
trimEnds x = drop 1 (take (numElements x - 1) x)

-- A nested list pretty much boils down to a tree
data Tree a = Node a | Branch [Tree a]
flattenList :: Tree a -> [a]
flattenList (Node x) = [x]
flattenList (Branch x) = concatMap flattenList x

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress [x,y] = if x == y then [x] else [x,y]
compress (x:xs) = if x == head xs then compress xs else [x] ++ compress xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = [[x] ++ (fst splitTuple)] ++ pack (snd splitTuple)
  where splitTuple = span (== x) xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (numElements x,head x)) (pack xs)

data CountStatus a = Single a | Multiple (Int, a)
  deriving (Eq, Show)
encodeModified :: (Eq a) => [a] -> [CountStatus a]
encodeModified xs = map (\x -> if numElements x == 1 then Single (head x) else Multiple (numElements x,head x)) (pack xs)

decodeModified :: [CountStatus a] -> [a]
decodeModified = concatMap helper
  where
    helper (Single x) = [x]
    helper (Multiple (n,x)) = replicate n x

encodeDirect :: (Eq a) => [a] -> [CountStatus a]
encodeDirect [] = []
encodeDirect xs = 
  let (headList,tailList) = span (==(head xs)) xs
    in helper headList ++ encodeDirect tailList
    where
      helper xs = if numElements xs > 1 then [Multiple (numElements xs,head xs)] else [Single (head xs)]
