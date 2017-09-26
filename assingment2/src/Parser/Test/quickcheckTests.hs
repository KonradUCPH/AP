module Main (main, spec) where
    
import Test.QuickCheck
import Test.Hspec
import qualified Data.Char as C

import SubsAst
import SubsParser

--import Control.Monad (liftM)

genJsString :: Gen String
genJsString = listOf $ elements $ [' '..'['] ++ [']'..'~'] -- printable acci characters minus '\'


instance Arbitrary Expr where
    arbitrary = expr

expr = sized exprN
exprN 0 = fmap Number arbitrary
exprN _ = oneof [ fmap Number arbitrary,
                  fmap String genJsString,
                  fmap Array arbitrary
                --, liftM Call "+" [subexpr, subexpr] -- we ran out of time here :(
                --, liftM2 Minus subexpr subexpr
                ]
            --where subexpr = exprN (n `div` 2)
-- checks for valid string characters
validJsString :: String -> Bool
validJsString = all $ \c -> c `elem` [' '..'~'] && (c /= '\\')

dummyTest :: Expr -> Bool
dummyTest _ = True

exprToJs :: Expr -> String
exprToJs (String s) = "\'" ++ s ++ "\'"
exprToJs (Number n) = show n
exprToJs (Array a) = arrayToJs a True
exprToJs _ = ""

arrayToJs :: [Expr] -> Bool -> String
arrayToJs a True = "[" ++ arrayToJs a False
arrayToJs [] False = "]"
arrayToJs [a] False = exprToJs a ++ "]"
arrayToJs (h:t) False = exprToJs h ++ ", " ++ arrayToJs t False

testRandomizedExpression :: Expr -> Bool
testRandomizedExpression expr = case parseString (exprToJs expr) of
            Right e -> e == expr
            Left _ -> True

main :: IO ()
main = hspec spec

-- Testcases
spec :: Spec
spec = do
  describe "Testing Random Expressions with QuickCheck" $ do
    it "Random Expression" $ property testRandomizedExpression