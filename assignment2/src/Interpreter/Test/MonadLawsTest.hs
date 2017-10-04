{- test to check the monad laws -}
module Main (main, spec) where

import Test.QuickCheck
import Test.Hspec

import SubsAst
import SubsInterpreter

--import Control.Monad (liftM)

instance Arbitrary Expr where
  arbitrary = expr

expr = sized exprN
exprN 0 = fmap Number arbitrary
exprN _ = oneof [ fmap Number arbitrary
                --, liftM Call "+" [subexpr, subexpr] -- we ran out of time here :(
                --, liftM2 Minus subexpr subexpr
                ]
  -- where subexpr = exprN (n `div` 2)

main :: IO ()
main = hspec spec

-- functions for testing associativity
f :: Expr -> SubsM Value
f = evalExpr

g :: a -> SubsM a
g = return


leftIdentity :: Expr -> Bool
leftIdentity expr = runSubsM (do x <- return (expr)
                                 f x 
                             ) initialContext 
                    ==
                    runSubsM (do f expr
                             ) initialContext  

rightIdentity :: Expr -> Bool
rightIdentity expr = runSubsM (do x <- f expr
                                  return x
                                  ) initialContext 
                     ==
                     runSubsM (do f expr
                     ) initialContext 

associativity :: Expr -> Bool
associativity expr =  runSubsM (do y <- do { x <- return expr;
                                             f x
                                           }
                                   g y
                               ) initialContext 
                      ==
                      runSubsM (do x <- return expr
                                   do { y <- f x;
                                        g y
                                      }
                      ) initialContext 
                                


-- Testcases
spec :: Spec
spec = do
  describe "Testing Monadic Laws" $ do
    it "Left Identity" $ property leftIdentity
    it "Right Identity" $ property rightIdentity
    it "Associativity" $ property associativity