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
testUndefined = case runExpr Undefined of
                  Right (UndefinedVal) -> True -- nothing to randomize here
                  _ -> False

testTrue :: Bool
testTrue = runExpr TrueConst == Right TrueVal 

testFalse :: Bool
testFalse = runExpr FalseConst == Right FalseVal 

testArray :: [Int] -> Bool
testArray xs = runExpr (Array (map (\x -> Number x) xs)) == Right ( ArrayVal (map (\x -> IntVal x) xs))

testVar :: String -> Int -> Bool
testVar name value = case runExpr (Comma (Assign name (Number value)) (Var name)) of
                       Right (IntVal read) -> read == value
                       _ -> False

testComma :: Int -> Int -> Bool
testComma x y = case runExpr (Comma (Number x) (Number y)) of
                  Right (IntVal read) -> read == y
                  _ -> False

testFunctionAdd :: Int -> Int -> Bool
testFunctionAdd x y = case runExpr (Call "+" [Number x, Number y]) of
                        Right (IntVal read) -> read == x + y
                        _ -> False


testFunctionWithWrongParams :: [Expr] -> Bool
testFunctionWithWrongParams xs = case runExpr (Call "+" xs) of
                                      Right _-> True
                                      Left _-> False

testStringConcat :: String -> String -> Bool
testStringConcat s1 s2 = case runExpr (Call "+" [String s1, String s2]) of
                              Right (StringVal s) -> s1 ++ s2 == s
                              _ -> False

testStringOrdering :: String -> String -> Bool
testStringOrdering s1 s2 = case runExpr (Call "<" [String s1, String s2]) of
                              Right TrueVal -> s1 < s2
                              Right FalseVal -> s1 >= s2
                              _ -> False

simpleArrayComprehension :: Either String Value
simpleArrayComprehension = runExpr (Comma (Assign "xs"
                                      (Array [Number 0, Number 1, Number 2, Number 3]))
                                      (Compr (ACFor "x" (Var "xs")
                                      (ACBody (Call "*" [Var "x", Var "x"]))))
                                    )
arrayComprehensionVariablePreservation :: Int -> Bool
arrayComprehensionVariablePreservation  i = runExpr (Comma (Assign "x" (Number i)) (Comma
                                            (Comma (Assign "xs"
                                            (Array [Number 0, Number 1, Number 2, Number 3]))
                                            (Compr (ACFor "x" (Var "xs")
                                            (ACBody (Call "*" [Var "x", Var "x"])))))
                                            (Var "x")
                                            )
                                         ) == (Right (IntVal i))

acCleanup :: Bool
acCleanup =  case runExpr (Comma
                    (Comma (Assign "xs"
                    (Array [Number 0, Number 1, Number 2, Number 3]))
                    (Compr (ACFor "x" (Var "xs")
                    (ACBody (Call "*" [Var "x", Var "x"])))))
                    (Var "x")
                    ) of
                      (Left _) -> True
                      (Right _) -> False

acNestedFor :: Either String Value
acNestedFor = runExpr (Compr (ACFor "i" (Array [Number 0]) (ACFor "x" (Call "Array" [Number 5]) (ACFor "y" (Call "Array" [Number 20]) (ACBody (Assign "i" (Call "+" [Var "i", Number 1])))))))

acIf :: Either String Value
acIf = runExpr (Comma (Assign "xs" (Array [Number 0, Number 1, Number 2, Number 3, Number 4, Number 5, Number 6, Number 7, Number 8, Number 9])) 
                (Compr (ACFor "x" (Var "xs") (ACIf (Call "===" [Call "%" [Var "x", Number 2], Number 0]) (ACBody (Var "x"))))))

acSingleExpression :: Either String Value
acSingleExpression = runExpr (Compr (ACBody (Number 42)))

acStrings :: String -> Bool
acStrings s = runExpr (Compr (ACFor "x" (String s) (ACBody (Var "x")))) == Right (ArrayVal [StringVal [y] | y <- s])

acStartingWithIfTrue :: Bool
acStartingWithIfTrue = runExpr (Compr (ACIf TrueConst (ACBody (Number 42)))) == runExpr (Compr (ACBody (Number 42)))

acStartingWithIfFalse :: Bool
acStartingWithIfFalse = runExpr (Compr (ACIf FalseConst (ACBody (Assign "x" (Number 42))))) == runExpr (Array [])

acNestedArrays :: Bool
acNestedArrays = runExpr (Comma (Assign "xs" (Array [Number 1, Number 2])) 
                         (Compr (ACFor "x" (Var "xs") (ACBody (Array [Var "x", Var "x"])))))
                 == Right (ArrayVal [ArrayVal [IntVal 1, IntVal 1], ArrayVal [IntVal 2, IntVal 2]])


-- Testcases
spec :: Spec
spec = do
  describe "Evaluation of basic Expressions" $ do
    it "Number to IntVal" $ property testNumber  -- unsing quickCheck to test function with random inputs
    it "String to StringVal" $ property testString
    it "Undefined expression" $ do testUndefined
    it "True expression" $ do testTrue
    it "False expression" $ do testFalse
  describe "Evaluation of combined Expressions" $ do
    it "Eval Array of numbers" $ property testArray
    it "Assign and Read variable" $ property testVar
    it "Evaluating Comma" $ property testComma
    it "Call Function" $ property testFunctionAdd
    it "Call Function wrong" $ testFunctionWithWrongParams [Number 1] `shouldBe` False
    it "Call Function right" $ testFunctionWithWrongParams [Number 1, Number 2] `shouldBe` True
    it "Call Function wrong" $ testFunctionWithWrongParams [Number 1, Number 2, Number 3] `shouldBe` False
  describe "Evaluation of complex expression" $ do
    it "Add to a variable" $
      runExpr (Comma (Assign "x" (Number 40)) (Call "+" [Var "x", Number 2])) `shouldBe` Right (IntVal 42)
    it "String Concat" $ property testStringConcat
    it "String Ordering" $ property testStringOrdering
  describe "Array Comprehensions" $ do
    it "Simple for Comprehension" $ simpleArrayComprehension `shouldBe` 
      (Right (ArrayVal [IntVal (x * x) | x <- [0..3]]))
    it "Variable preservation in Comprehension" $ property arrayComprehensionVariablePreservation
    it "Variables are cleaned up after exiting a block of code" $ acCleanup
    it "Nested Fors in Array comprehension" $ acNestedFor `shouldBe` (Right ( ArrayVal [IntVal x | x <- [1..100]]))
    it "Array comprehension with If" $ acIf `shouldBe` (Right ( ArrayVal [IntVal x | x <- [0..9], x `mod` 2 == 0]))
    it "Array comprehension with only one expression" $ acSingleExpression `shouldBe` (Right (IntVal 42))
    it "Array comprehension with Strings" $ property acStrings
    it "Array comprehension with if clause first, evaluating to true" $ acStartingWithIfTrue
    it "Array comprehension with if clause first, evaluating to false" $ acStartingWithIfFalse
    it "Array comprehension with nested Arrays" $ acNestedArrays
    