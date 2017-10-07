module Main (main, spec) where
    
import Test.QuickCheck
import Test.Hspec
import qualified Data.Char as C
import Control.Monad

import SubsAst
import SubsParser

instance Arbitrary Expr where
    arbitrary = sized arbitraryExpr

arbitraryExpr :: Int -> Gen Expr
arbitraryExpr n = oneof [
                            simpleExpr,
                            recExpr n
                        ]                   

-- non recursive expressions
simpleExpr :: Gen Expr
simpleExpr = oneof [ fmap Number arbitrary,
                     fmap String arbitraryJsString,
                     return Undefined,
                     return TrueConst,
                     return FalseConst,
                     fmap Var arbitraryJsIdent
                   ]

-- recursive expressions
recExpr :: Int -> Gen Expr
recExpr 0 = simpleExpr
recExpr n = oneof [ arbitraryArray n,
                    arbitraryCall n,
                    arbitraryAssign n
                  ]
            --where subexpr = exprN (n `div` 2)

-- generates an Arbitrary Array expression, limited by length n
arbitraryArray :: Int -> Gen Expr
arbitraryArray n = do
                    (Positive m) <- arbitrary
                    let n' = n `div` (m + 1)
                    a <- replicateM m (arbitraryExpr n') -- get array of expressions
                    return $ Array a

arbitraryCall :: Int -> Gen Expr
arbitraryCall n = do
                    (Positive m) <- arbitrary -- get positive number
                    let n' = n `div` (m + 1)
                    a <- replicateM m (arbitraryExpr n')  -- get array of expressions
                    i <- arbitraryJsIdent
                    return $ Call i a

arbitraryAssign :: Int -> Gen Expr
arbitraryAssign n = do
                        (Positive m) <- arbitrary -- get positive number
                        let n' = n `div` (m + 1)
                        e <- arbitraryExpr n'
                        i <- arbitraryJsIdent
                        return $ Assign i e


-- generate arbitrary valid JavaScript string
-- ghci: call $ generate arbitraryJsString
arbitraryJsString :: Gen String
arbitraryJsString = listOf $ elements $ [' '..'&'] 
        ++ ['('..'['] 
        ++ [']'..'~'] -- printable asci characters minus '\' and '''

arbitraryJsIdent :: Gen String
arbitraryJsIdent = suchThat (listOf $ elements ['a'..'z']) (\s -> length s > 0) -- at least one character


-- transform an expression into JavaScript code
exprToJs :: Expr -> String
exprToJs (String s) = "\'" ++ s ++ "\'"
exprToJs (Number n) = show n
exprToJs (Var i) = i
exprToJs TrueConst = "true"
exprToJs FalseConst = "false"
exprToJs Undefined = "undefined"
exprToJs (Assign i e) = i ++ " = " ++ exprToJs e
exprToJs (Call i a) = i ++ arrayToJs a True "(" ")"
exprToJs (Array a) = arrayToJs a True "[" "]"
exprToJs _ = undefined

-- params: list of expr, is this the first elem?, start sign, end sign
arrayToJs :: [Expr] -> Bool -> String -> String -> String
arrayToJs a True s e = s ++ arrayToJs a False s e
arrayToJs [] False _ e = e
arrayToJs [a] False _ e = exprToJs a ++ e
arrayToJs (h:t) False s e = exprToJs h ++ ", " ++ arrayToJs t False s e



testRandomizedExpression :: Expr -> Bool
testRandomizedExpression expr = case parseString (exprToJs expr) of
            Right e -> e == expr
            Left _ -> False

main :: IO ()
main = hspec spec

-- Testcases
spec :: Spec
spec = do
  describe "Testing Random Expressions with QuickCheck" $ do
    it "Random Expression" $ property testRandomizedExpression