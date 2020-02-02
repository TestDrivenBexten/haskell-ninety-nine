import Test.Hspec
import ListNinetyNine

main :: IO ()
main = hspec $ do
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