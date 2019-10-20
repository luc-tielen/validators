module Data.ValidatorSpec (module Data.ValidatorSpec) where

import Data.Validation
import Data.Validator
import Test.Hspec

(==>) :: (Eq a, Show a) => a -> a -> IO ()
(==>) = shouldBe

infixr 0 ==>

data Error = TooSmall | TooBig | WrongNumber
  deriving (Eq, Show)

spec :: Spec
spec = describe "Validators" $ parallel $ do
  describe "Running validators" $ parallel $ do
    it "can run single validation on a subject" $ do
      let validator = assert (> 10) [TooSmall]
      validate validator 11 ==> Success 11
      validate validator 1 ==> Failure [TooSmall]
    it "can run multiple validations on a subject" $ do
      let validator =
            assert (< 10) [TooBig]
              <> assert (== 7) [WrongNumber]
      validate validator 7 ==> Success 7
      validate validator 6 ==> Failure [WrongNumber]
      validate validator 11 ==> Failure [TooBig, WrongNumber]
