import Control.Exception
import Test.Hspec
import ListNinetyNine

main :: IO ()
main = hspec $ do
    describe "When finding the last element of a list" $ do
        describe "Given an empty list" $ do
            let emptyList = []
            it "Should throw an error" $ do
                lastInList emptyList `shouldThrow` anyException

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
        let emptyList = []
        it "An empty list should be an empty list" $do
            length (reverseList emptyList) `shouldBe` 0

        it "The list [5] should be [5]" $ do
            reverseList [5] `shouldBe` [5]

        it "The list [4,5] should be [5,4]" $ do
            reverseList [4,5] `shouldBe` [5,4]

        it "The list [3,4,5] should be [5,4,3]" $ do
            reverseList [3,4,5] `shouldBe` [5,4,3]

        it "The String \"Haskell\" should be \"lleksaH\"" $ do
            reverseList "Haskell" `shouldBe` "lleksaH"

    describe "When checking for a palindome" $ do
        it "An empty list is a palindrome" $ do
            isPalindrome [] `shouldBe` True

        it "A list with one number is a palindrome" $ do
            isPalindrome [5] `shouldBe` True

        it "A single character is a palindrome" $ do
            isPalindrome "a" `shouldBe` True

        it "The word \"cat\" is not a palindrome" $ do
            isPalindrome "cat" `shouldBe` False

        it "The word \"bib\" is a palindrome" $ do
            isPalindrome "bib" `shouldBe` True

        it "A list with the numbers [3,4,5] is not a palindrome" $ do
            isPalindrome [3,4,5] `shouldBe` False

        it "A list with the numbers [6,2,6] is a palindrome" $ do
            isPalindrome [6,2,6] `shouldBe` True

        it "The phrase \"borrow or rob\" is a palindrome" $ do
            isPalindrome "borrow or rob" `shouldBe` True