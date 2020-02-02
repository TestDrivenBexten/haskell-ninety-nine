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
        
        -- it "A list with one element should throw an error" $ do
        --     penultimateInList [5] `shouldThrow` anyException

        it "A list with two elements should return the first element" $ do
            penultimateInList [7,8] `shouldBe` 7

        it "A list with three elements should return the second element" $ do
            penultimateInList [5,6,7] `shouldBe` 6