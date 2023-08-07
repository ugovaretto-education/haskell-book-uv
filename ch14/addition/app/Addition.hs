module Addition where
import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

oneThroughThree :: Gen Int
oneThroughThree =
  elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)
genBool' :: Gen Bool
genBool' = elements [False, True]
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

firstTest :: IO ()
firstTest = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1+1) > 1 `shouldBe` True
    it "x + 1 is always\
        \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x
runQc :: IO ()
runQc = quickCheck prop_additionGreater
