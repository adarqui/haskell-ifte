module Data.IfteSpec (
  main,
  spec
) where



import Test.Hspec
import Data.Ifte



main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  describe "ifte" $ do
    it "if then else" $ do
      ifte True one two  `shouldBe` one
      ifte False one two `shouldBe` two

  describe "ifet" $ do
    it "if else then" $ do
      ifet True two one  `shouldBe` one
      ifet False two one `shouldBe` two

  describe "teif" $ do
    it "then else, if" $ do
      teif one two True  `shouldBe` one
      teif one two False `shouldBe` two

  describe "etif" $ do
    it "else then, if" $ do
      etif two one True  `shouldBe` one
      etif two one False `shouldBe` two

  describe "ifteEither" $ do
    it "if then Right else Left" $ do
      ifteEither True one two  `shouldBe` (Right one)
      ifteEither False one two `shouldBe` (Left two)

  describe "ifetEither" $ do
    it "if else Left then Right" $ do
      ifetEither True two one  `shouldBe` (Right one)
      ifetEither False two one `shouldBe` (Left two)

  describe "teifEither" $ do
    it "then Right else Left, if" $ do
      teifEither one two True  `shouldBe` (Right one)
      teifEither one two False `shouldBe` (Left two)

  describe "etifEither" $ do
    it "else Left then Right, if" $ do
      etifEither two one True  `shouldBe` (Right one)
      etifEither two one False `shouldBe` (Left two)

  describe "ifMaybe" $ do
    it "if then Just else Nothing" $ do
      ifMaybe True one  `shouldBe` (Just one)
      ifMaybe False one `shouldBe` Nothing

  describe "fiMaybe" $ do
    it "then Just else Nothing, if" $ do
      fiMaybe one True  `shouldBe` (Just one)
      fiMaybe one False `shouldBe` Nothing



one :: Int
one = 1



two :: Int
two = 2
