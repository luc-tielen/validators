module Data.ValidationSpec (module Data.ValidationSpec) where

import Control.Arrow ((>>>))
import Data.Validation
import Data.Validator
import Test.Hspec

(==>) :: (Eq a, Show a) => a -> a -> IO ()
(==>) = shouldBe

infixr 0 ==>

type Name = String

type Age = Int

data Person = Person Name Age
  deriving (Eq, Show)

spec :: Spec
spec = describe "Running validations" $ parallel $ do
  it "can combine validations into bigger validations" $ do
    let nameValidator = check (length >>> (> 0)) ["name can't be empty"]
        ageValidator =
          check (> 0) ["age must be > 0"]
            <> check (< 120) ["age must be < 120"]
        runTest name age =
          Person <$> validate nameValidator name <*> validate ageValidator age
    runTest "" 0 ==> Failure ["name can't be empty", "age must be > 0"]
    runTest "" 10 ==> Failure ["name can't be empty"]
    runTest "Alice" 0 ==> Failure ["age must be > 0"]
    runTest "Alice" 25 ==> Success $ Person "Alice" 25
