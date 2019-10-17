module Data.ValidationSpec ( module Data.ValidationSpec ) where

import Test.Hspec
import Data.Validation
import Control.Arrow ( (>>>) )


(==>) :: (Eq a, Show a) => a -> a -> IO ()
(==>) = shouldBe

infixr 0 ==>

data Error = TooSmall | TooBig | WrongNumber
  deriving (Eq, Show)

type Name = String
type Age = Int
data Person = Person Name Age
  deriving (Eq, Show)

spec :: Spec
spec = describe "Running validations" $ parallel $ do
  describe "validators" $ parallel $ do
    it "can run single validation on a subject" $ do
      let validator = check (> 10) [TooSmall]
      fmap fromValid (runValidation (validate validator 11)) ==> Right 11
      runValidation (validate validator 1) ==> Left [TooSmall]

    it "can run multiple validations on a subject" $ do
      let validator =  check (< 10) [TooBig]
                    <> check (== 7) [WrongNumber]
      fmap fromValid (runValidation (validate validator 7)) ==> Right 7
      runValidation (validate validator 6) ==> Left [WrongNumber]
      runValidation (validate validator 11) ==> Left [TooBig, WrongNumber]

  it "can combine validations into bigger validations" $ do
    let nameValidator = check (length >>> (> 0)) ["name can't be empty"]
        ageValidator = check (> 0) ["age must be > 0"]
                    <> check (< 120) ["age must be < 120"]
        runTest name age =  runValidation
                         $  Person
                        <$> validate nameValidator name
                        <*> validate ageValidator age
    runTest "" 0 ==> Left ["name can't be empty", "age must be > 0"]
    runTest "" 10 ==> Left ["name can't be empty"]
    runTest "Alice" 0 ==> Left ["age must be > 0"]
    fmap fromValid (runTest "Alice" 25) ==> Right $ Person "Alice" 25

