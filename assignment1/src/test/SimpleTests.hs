{- Simple tests to test basic program functionallity -}
module Main (main, spec) where

import Test.QuickCheck
import Test.Hspec

import SubsAst
import SubsInterpreter

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

testNumber :: Int -> Bool
testNumber x = runExpr (Number x) == Right (IntVal x)

testString :: String -> Bool
testString s = runExpr (String s) == Right (StringVal s)

testUndefined :: Bool
testUndefined = runExpr Undefined == Left _ -- nothing to randomize here

testArray :: [Int] -> Bool
testArray xs = runExpr (Array (map (\x -> Number x) xs)) == Right ( ArrayVal (map (\x -> IntVal x) xs))

-- Testcases
spec :: Spec
spec = do
  describe "Evaluation of basic Expressions" $ do
    it "Number to IntVal" $ property testNumber  -- unsing quickCheck to test function with random inputs
    it "String to StringVal" $ property testString
    it "Undefined expression" $ do testUndfined
  describe "Evaluation of combined Expressions" $ do
    it "array of numbers" $ property testArray