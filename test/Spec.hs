import Test.Hspec

main :: IO ()
main = hspec $ do
    it "Given an empty list" $ do
        2 `shouldBe` 3