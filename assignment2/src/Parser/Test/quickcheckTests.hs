module Main (main, spec) where
    
import Test.QuickCheck
import Test.Hspec
import qualified Data.Char as C
import Control.Monad

import SubsAst
import SubsParser

--import Control.Monad (liftM)




instance Arbitrary Expr where
    arbitrary = sized exprN

exprN 0 = fmap Number arbitrary
exprN n = oneof [ fmap Number arbitrary,
                  fmap String genJsString,
                  arbitraryArray n
                ]
            --where subexpr = exprN (n `div` 2)

-- generates an Arbitrary Array expression, limited by length n
arbitraryArray :: Int -> Gen Expr
arbitraryArray n = do
                    (Positive m) <- arbitrary -- get positive number
                    let n' = n `div` (m + 1)
                    a <- replicateM m (exprN n')
                    return $ Array a

-- generate arbitrary valid JavaScript string
arbitraryJsString :: Gen String
arbitraryJsString = listOf $ elements $ [' '..'['] ++ [']'..'~'] -- printable asci characters minus '\'

-- transform an expression into JavaScript code
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