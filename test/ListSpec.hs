module ListSpec (spec) where

import Control.Exception
import Test.Hspec
import ListNinetyNine

spec :: Spec
spec = do
    describe "When finding the last element of a list" $ do
        describe "Given an empty list" $ do
            it "Should throw an error" $ do
                lastInList [] `shouldThrow` anyException

        describe "Given a list of fractionals" $ do
            let fractionalList = [2.5,5.6,8.9]
            it "Should return the last fractional" $ do
                lastInList fractionalList `shouldBe` 8.9

        describe "Given a list of integers" $ do
            let numberList = [4,5,6]
            it "The last number should be returned" $ do
                lastInList numberList `shouldBe` 6

    describe "When finding the penultimate element in a list" $ do
        it "An empty list should throw an error" $ do
            penultimateInList [] `shouldThrow` anyException
        
        it "A list with one element should throw an error" $ do
            evaluate (penultimateInList [5]) `shouldThrow` anyException

        it "A list with two elements should return the first element" $ do
            penultimateInList [7,8] `shouldBe` 7

        it "A list with three elements should return the second element" $ do
            penultimateInList [5,6,7] `shouldBe` 6

    describe "When finding the kth element in a list" $ do
        it "An empty list should throw an error" $ do
            kthInList [] 1 `shouldThrow` anyException

        it "A list with one element should throw an error for k=2" $ do
            evaluate (kthInList [5] 2) `shouldThrow` anyException

        it "A list with one element should throw an error for k=0" $ do
            evaluate (kthInList [5] 0) `shouldThrow` anyException

        it "A list with one element should return the element for k=1" $ do
            kthInList [5] 1 `shouldBe` 5
        
        it "A list with two elements should return the second element for k=2" $ do
            kthInList [8,3] 2 `shouldBe` 3

        it "The String \"Haskell\" should return 'e' for k=5" $ do
            kthInList "Haskell" 5 `shouldBe` 'e'

    describe "When finding the number of elements in a list" $ do
        it "An empty list should return 0" $ do
            numElements [] `shouldBe` 0

        it "A list with one number should return 1" $ do
            numElements [7] `shouldBe` 1

        it "The String \"Haskell\" should return 7" $ do
            numElements "Haskell" `shouldBe` 7

        it "The String \"Hello, World!\" should return 13" $ do
            numElements "Hello, World!" `shouldBe` 13

    describe "When reversing a list" $ do
        it "An empty list should be an empty list" $do
            length (reverseList ([] :: [Int])) `shouldBe` 0

        it "The list [5] should be [5]" $ do
            reverseList [5] `shouldBe` [5]

        it "The list [4,5] should be [5,4]" $ do
            reverseList [4,5] `shouldBe` [5,4]

        it "The list [3,4,5] should be [5,4,3]" $ do
            reverseList [3,4,5] `shouldBe` [5,4,3]

        it "The String \"Haskell\" should be \"lleksaH\"" $ do
            reverseList "Haskell" `shouldBe` "lleksaH"

    describe "When checking for a palindome" $ do
        describe "When trimming the ends off a word" $ do
            it "An empty String should be an empty String" $ do
                trimEnds "" `shouldBe` ""
            
            it "The character 'a' should be ''" $ do
                trimEnds "a" `shouldBe` ""

            it "The word \"no\" should be ''" $ do
                trimEnds "no" `shouldBe` ""

            it "The word \"cat\" should be 'a'" $ do
                trimEnds "cat" `shouldBe` "a"

        it "An empty list should throw an exception" $ do
            isPalindrome ([] :: [Int]) `shouldBe` True

        it "A list with one number is a palindrome" $ do
            isPalindrome [5] `shouldBe` True

        it "A single character is a palindrome" $ do
            isPalindrome "a" `shouldBe` True

        it "A list with the numbers [5,5] is a palindrome" $ do
            isPalindrome [5,5] `shouldBe` True

        it "A list with the numbers [3,7] is not a palindrome" $ do
            isPalindrome [3,7] `shouldBe` False

        it "The word \"aa\" is a palindrome" $ do
            isPalindrome "aa" `shouldBe` True

        it "The word \"no\" is not a palindrome" $ do
            isPalindrome "no" `shouldBe` False

        it "The word \"cat\" is not a palindrome" $ do
            isPalindrome "cat" `shouldBe` False

        it "The word \"bib\" is a palindrome" $ do
            isPalindrome "bib" `shouldBe` True

        it "A list with the numbers [3,4,5] is not a palindrome" $ do
            isPalindrome [3,4,5] `shouldBe` False

        it "A list with the numbers [6,2,6] is a palindrome" $ do
            isPalindrome [6,2,6] `shouldBe` True

        it "The word \"borroworrob\" is a palindrome" $ do
            isPalindrome "borroworrob" `shouldBe` True

    describe "When flattening a nested list" $ do
        it "A node 5 should be [5]" $ do
            flattenList (Node 5) `shouldBe` [5]
        
        it "The branch [] should be []" $ do
            flattenList (Branch []) `shouldBe` ([] :: [Int])

        it "The branch [5] should be [5]" $ do
            flattenList (Branch [Node 5]) `shouldBe` [5]

        it "The [7,[],5] should be [7,5]" $ do
            flattenList (Branch [(Node 7),(Branch []),(Node 5)]) `shouldBe` [7,5]

        it "The list [[6,9],[8,5,[2],[3,1]]] should be [6,9,8,3,1]" $ do
            flattenList (Branch [
                    Branch[(Node 6),(Node 9)],
                    Branch[(Node 8),(Node 5),Branch[(Node 2)],
                        Branch[(Node 3),(Node 1)]]
                ]) `shouldBe` [6,9,8,5,2,3,1]

    describe "When compressing a list with duplicates" $ do
        it "An empty list returns an empty list" $ do
            compress ([] :: [Int]) `shouldBe` ([] :: [Int])
        
        it "The list [1] returns [1]" $ do
            compress [1] `shouldBe` [1]

        it "The list [1,1] returns [1]" $ do
            compress [1,1] `shouldBe` [1]

        it "The list [1,2] returns [1,2]" $ do
            compress [1,2] `shouldBe` [1,2]

        it "The list [1,2,2] returns [1,2]" $ do
            compress [1,2,2] `shouldBe` [1,2]

        it "aaaabccaadeeee should be compressed to abcade" $ do
            compress "aaaabccaadeeee" `shouldBe` "abcade"

    describe "When packing a list with duplicates" $ do
        it "An empty list returns an empty list" $ do
            pack ([] :: [Int]) `shouldBe` ([] :: [[Int]])

        it "The list [1] should be [[1]]" $ do
            pack [1] `shouldBe` [[1]]

        it "The list [1,2] should be [[1],[2]]" $ do
            pack [1,2] `shouldBe` [[1],[2]]

        it "The list [1,1] should be [[1,1]]" $ do
            pack [1,1] `shouldBe` [[1,1]]

        it "aaaabccaadeeee should be [aaaa,b,cc,aa,d,eeee]" $ do
            pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

    describe "When encoding a list with duplicates" $ do
        it "An empty list returns an empty list" $ do
            encode ([] :: [Int]) `shouldBe` ([] :: [(Int,Int)])

        it "The list [5] should return [(1,5)]" $ do
            encode [5] `shouldBe` [(1,5)]

        it "aaaabccaadeeee should be [(4,a),(1,b),(2,c),(2,a),(1,d),(4,e)]" $ do
            encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    describe "When encoding a list with statuses" $ do
        it "abc should be [Single a, Single b, Single c]" $ do
            encodeModified "abc" `shouldBe` [Single 'a',Single 'b',Single 'c']

        it "aaaabccaadeeee should be [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']" $ do
            encodeModified "aaaabccaadeeee" `shouldBe` [Multiple (4,'a'),Single 'b',Multiple (2,'c'),Multiple (2,'a'),Single 'd',Multiple (4,'e')]

    describe "When decoding a list with statuses" $ do
        it "[Single a, Single b, Single c] should be abc" $ do
            decodeModified [Single 'a',Single 'b',Single 'c'] `shouldBe` "abc"

        it "[Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] should be aaaabccaadeeee" $ do
            decodeModified [Multiple (4,'a'),Single 'b',Multiple (2,'c'),Multiple (2,'a'),Single 'd',Multiple (4,'e')] `shouldBe` "aaaabccaadeeee"

    describe "When encoding a list with duplicates without grouping" $ do
        it "The list [5] should return [Single 5]" $ do
            encodeDirect [5] `shouldBe` [Single 5]

        it "aaaabccaadeeee should be [(4,a),(1,b),(2,c),(2,a),(1,d),(4,e)]" $ do
            encodeDirect "aaaabccaadeeee" `shouldBe` [Multiple (4,'a'),Single 'b',Multiple (2,'c'),Multiple (2,'a'),Single 'd',Multiple (4,'e')]

    describe "When duplicating the elements in a list" $ do
        it "An empty list should be an empty list" $ do
            dupli ([] :: [Int]) `shouldBe` ([] :: [Int])

        it "The word beer should be bbeeeerr" $ do
            dupli "beer" `shouldBe` "bbeeeerr"

        it "The list [1,2,3] should be [1,1,2,2,3,3]" $ do
            dupli [1,2,3] `shouldBe` [1,1,2,2,3,3]
            
    describe "When replicating the elements in a list" $ do
        it "The empty list should be an empty list" $ do
            repli ([] :: [Int]) 3 `shouldBe` ([] :: [Int])

        it "Replicating the word abc 3 times should be aaabbbccc" $ do
            repli "abc" 3 `shouldBe` "aaabbbccc"

        it "Replicating [1,2,3] 2 times should be [1,1,2,2,3,3]" $ do
            repli [1,2,3] 2 `shouldBe` [1,1,2,2,3,3]

        it "Replicating [1,2,3] 0 times should be an empty list" $ do
            repli [1,2,3] 0 `shouldBe` []
    
    describe "When dropping every nth element from a list" $ do
        describe "An n value of 1 should return original list" $ do
            it "The word apple should be ''" $ do
                dropEvery "apple" 1 `shouldBe` ""

            it "The list [1,2,3] should be []" $ do
                dropEvery [1,2,3] 1 `shouldBe` ([] :: [Int])

        it "The word apple dropping every fifth element should be \"appl\"" $ do
            dropEvery "apple" 5 `shouldBe` "appl"

        it "The word apple dropping every sixth element should be \"apple\"" $ do
            dropEvery "apple" 6 `shouldBe` "apple"

        it "The sequence \"abcdefghik\" dropping every third element should be \"abdeghk\"" $ do
            dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

    describe "When splitting a list into two parts" $ do
        it "The list [1,2,3] split at position 3 will be [[1,2,3],[]]" $ do
            splitList [1,2,3] 3 `shouldBe` ([1,2,3],[])

        it "abcdef split at position 3 will be abc and def" $ do
            splitList "abcdef" 3 `shouldBe` ("abc","def")

    describe "When slicing a subset from a list" $ do
        it "The list [10,20,30,40,50] should be [20,30,40] from 2 to 4" $ do
            sliceList [10,20,30,40,50] 2 4 `shouldBe` [20,30,40]

        it "The word \"terror\" should be \"error\" from 2 to 6" $ do
            sliceList "terror" 2 6 `shouldBe` "error"

    describe "When rotating a list n places to the left" $ do
        it "The word \"polka\" shifted left 2 becomes \"lkapo\"" $ do
            rotateList "polka" 2 `shouldBe` "lkapo"

        it "The list [3,5,7,11,13] shifted right 1 becomes [13,3,5,7,11]" $ do
            rotateList [3,5,7,11,13] (-1) `shouldBe` [13,3,5,7,11]

    describe "When removing an element in a list" $ do
        it "The word \"splice\" should be \"slice\" removing the second letter" $ do
            removeElement 2 "splice" `shouldBe` "slice"

        it "The list [4,5,6] should be [4,5] removing the third element" $ do
            removeElement 3 [4,5,6] `shouldBe` [4,5]

    describe "When inserting an element into a list" $ do
        it "The number 5 should be inserted into the empty list at 1" $ do
            insertAt 5 [] 1 `shouldBe` [5]

        it "The letter x inserted at 2" $ do
            insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

    describe "When creating a range of integers" $ do
        it "The range 1 to 3 should be [1,2,3]" $ do
            range 1 3 `shouldBe` [1,2,3]

        it "The range 4 to 9 should be [4,5,6,7,8,9]" $ do
            range 4 9 `shouldBe` [4,5,6,7,8,9]

    describe "When selecting n elements randomly" $ do
        it "Randomly selecting 3 from [1,2,3,4,5,6,7]" $ do
            randomSelect (range 1 7) 3 `shouldBe` [1,7,5]

    describe "When drawing n numbers from the set 1..M" $ do
        it "Randomly select six elements up to 49" $ do
            randomLotto 6 49 `shouldBe` [15,37,9,26,43,8]

    describe "When shuffling a list" $ do
        it "Shuffle 1 to 5" $ do
            shuffleList (range 1 5) `shouldBe` [4,5,3,2,1]

        it "Shuffle abcde" $ do
            shuffleList "abcde" `shouldBe` "decba"

    describe "When getting all combinations of a list" $ do
        it "k=1 for abc should have 3 permutations" $ do
            combinations 1 "abc" `shouldBe` ["a","b","c"]

        it "k=2 for abc should have 3 permutations" $ do
            combinations 2 "abc" `shouldBe` ["ab","ac","bc"]

        it "k=2 for abcd should have 6 permutations" $ do
            length (combinations 2 "abcd") `shouldBe` 6

        it "k=3 for a list of 12 should have 220 permutations" $ do
            length (combinations 3 (range 1 12)) `shouldBe` 220

    describe "When getting all disjoint subsets" $ do
        it "group (3) for 123 should give 123" $ do
            groupDisjoint [3] [1,2,3] `shouldBe` [[[1,2,3]]]
        
        it "group (3) for [ant,bat,cat] should give [ant,bat,cat]" $ do
            groupDisjoint [3] ["ant","bat","cat"] `shouldBe` [[["ant","bat","cat"]]]

        it "group (1,2) for [1,2,3] should give [[[1],[2,3]],[[2],[1,3]],[[3],[1,2]]]" $ do
            groupDisjoint [1,2] [1,2,3] `shouldBe` [[[1],[2,3]],[[2],[1,3]],[[3],[1,2]]]

        it "group (2,3,4) for [abcdefghi] should give 1260 permutations" $ do
            length (groupDisjoint [2,3,4] "abcdefghi") `shouldBe` 1260

    describe "When sorting lists by length" $ do
        it "The list a,do should be a,do" $ do
            lsort ["a","do"] `shouldBe` ["a","do"]

        it "The list do,a should be a,do" $ do
            lsort ["do","a"] `shouldBe` ["a","do"]

        it "The list abc,de,fgh,de,ijkl,mn,o should be o,de,de,mn,abc,fgh,ijkl" $ do
            lsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["o","de","de","mn","abc","fgh","ijkl"]

    describe "When sorting lists by length" $ do
        it "The list be,a,do should be a,be,do" $ do
            lfsort ["be","a","do"] `shouldBe` ["a","be","do"]

        it "The list abc,de,fgh,de,ijkl,mn,o should be ijkl,o,abc,fgh,de,de,mn" $ do
            lfsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["ijkl","o","abc","fgh","de","de","mn"]