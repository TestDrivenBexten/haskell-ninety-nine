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
      encodeDirect,
      dupli,
      repli,
      dropEvery,
      splitList,
      sliceList,
      rotateList,
      removeElement,
      insertAt,
      range,
      randomSelect,
      randomLotto,
      shuffleList,
      combinations,
      groupDisjoint,
      lsort,
      lfsort
    ) where

import qualified Data.List as List
import System.Random

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
numElements = foldl (\count _ -> count + 1) 0

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
compress (x:xs)
  | x == head xs = compress xs
  | otherwise = [x] ++ compress xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = [duplicate] ++ pack remainder
  where (duplicate,remainder) = span (== head xs) xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (numElements x,head x)) (pack xs)

data CountStatus a = Single a | Multiple (Int, a)
  deriving (Eq, Show)

listToCountStatus :: [a] -> CountStatus a
listToCountStatus xs
  | numElements xs == 1 = Single (head xs)
  | otherwise = Multiple (numElements xs,head xs)

encodeModified :: (Eq a) => [a] -> [CountStatus a]
encodeModified xs = map listToCountStatus (pack xs)

decodeModified :: [CountStatus a] -> [a]
decodeModified = concatMap helper
  where
    helper (Single x) = [x]
    helper (Multiple (n,x)) = replicate n x

encodeDirect :: (Eq a) => [a] -> [CountStatus a]
encodeDirect [] = []
encodeDirect xs = 
  let (headList,tailList) = span (==(head xs)) xs
    in [listToCountStatus headList] ++ encodeDirect tailList

dupli :: [a] -> [a]
dupli = foldr (\x list -> x:x:list) []

repli :: [a] -> Int -> [a]
repli xs n = concatMap (take n . repeat) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n =
  let (headList,tailList) = splitAt n xs
    in take (n - 1) headList ++ dropEvery tailList n

splitList :: [a] -> Int -> ([a],[a])
splitList xs n = (take n xs, drop n xs)

sliceList :: [a] -> Int -> Int -> [a]
sliceList xs start end = drop (start - 1) (take end xs)

rotateList :: [a] -> Int -> [a]
rotateList xs n =
  let (headList,tailList) = splitList xs $ (length xs + n) `mod` (length xs)
    in tailList ++ headList

removeElement :: Int -> [a] -> [a]
removeElement n xs = take (n - 1) xs ++ drop n xs

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n =
  let (headList,tailList) = splitList xs (n - 1)
    in headList ++ [x] ++ tailList

range :: Int -> Int -> [Int]
range start end = [start..end]

randomSelect :: [a] -> Int -> [a]
randomSelect xs n
    | n > 0 = [kthInList xs randomIndex] ++ randomSelect (removeElement randomIndex xs) (n - 1)
    | otherwise = []
  where (randomIndex,_) = randomR (1,length xs) (mkStdGen 0)

randomLotto :: Int -> Int -> [Int]
randomLotto n m = randomSelect (range 1 m) n

shuffleList :: [a] -> [a]
shuffleList xs = randomSelect xs (length xs)

combinations :: Int -> [a] -> [[a]]
combinations 1 xs = map (\x -> [x]) xs
combinations n (x:xs)
  | n < length (x:xs) = map (x:) (combinations (n - 1) xs) ++ combinations n xs
  | n == length (x:xs) = [x:xs]
  | otherwise = [[]]

groupDisjoint :: (Eq a) => [Int] -> [a] -> [[[a]]]
groupDisjoint [] xs = [[xs]]
groupDisjoint [n] xs = [combinations n xs]
groupDisjoint (n:ns) xs =
  [ [rs] ++ gs | rs <- combinations n xs,
           gs <- groupDisjoint ns (removeElements rs xs)]

removeElements :: (Eq a) => [a] -> [a] -> [a]
removeElements elemList xs = [ x | x <- xs, not(x `elem` elemList)]

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = lsort shortSubList ++ [x] ++ lsort longSubList
  where
    shortSubList = filter (\y -> length y < length x) xs
    longSubList = filter (\y -> length y >= length x) xs

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort xs = [ x | y <- orderedLengthList, x <- xs, y == length x]
  where
    orderedLengthList = compress $ map snd sortedFreqLengthList
    sortedFreqLengthList = List.sortBy compareFrequencyLength frequencyLengthList
    frequencyLengthList = encode $ map length $ lsort xs

compareFrequencyLength :: (Int,Int) -> (Int,Int) -> Ordering
compareFrequencyLength (xFreq,_) (yFreq,_)
  | xFreq < yFreq = LT
  | otherwise = GT