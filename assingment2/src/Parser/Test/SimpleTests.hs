module Main (main, spec) where

import Test.QuickCheck
import Test.Hspec

import SubsAst
import SubsInterpreter
import Parser.Impl
import SubsParser

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


shouldParseTo :: String -> Expr -> Bool
shouldParseTo js e = case parseString js of
                            Right er -> er == e
                            Left _ -> False 

shouldFail :: String -> Bool
shouldFail js = case parseString js of
                Right _ -> False
                Left _ -> True

spec :: Spec
spec = do
  describe "Evaluation of basic Expressions" $ do
    it "JS Number to Number" $ "42" `shouldParseTo` (Number 42)
    it "Variable assingment" $ "a=42" `shouldParseTo` (Assign "a"(Number 42))
    it "Comma test" $ "4,2" `shouldParseTo` (Comma (Number 4)(Number 2))
    it "White Space trimming" $ " a     = 42 " `shouldParseTo` (Assign "a"(Number 42))
    it "Comment test" $ "true // true=5" `shouldParseTo` (TrueConst) 
    it "Reserved world var" $ shouldFail "true=5"
    it "Almost Reserved world var" $ "true1=42" `shouldParseTo` (Assign "true1"(Number 42))
 
  describe "String tests" $ do
    it "Simple string" $ "'Test 42'" `shouldParseTo` (String "Test 42")
    it "Reserved world var" $ shouldFail "'test42"
    it "Reserved world in string" $ "'true'" `shouldParseTo` (String "true")
    it "Quote escaping" $ "'Test\\''" `shouldParseTo` (String "Test\'")
    it "New line character escaping" $ "'Test\\n'" `shouldParseTo` (String "Test\n")
    it "Tab escaping" $ "'Test\\t'" `shouldParseTo` (String "Test\t")
    it "BackSlash escaping" $ "'Test\\\\'" `shouldParseTo` (String "Test\\")
    it "BackSlash Newline escaping" $ "'Te\\\nst'" `shouldParseTo` (String "Test")

  describe "Evaluation of basic arithmetic operations" $ do
    it "Add operation" $ "2+2" `shouldParseTo` (Call "+" [Number 2, Number 2])
    it "Operators precedence * and +" $ "1+2*3" `shouldParseTo` (Call "+" [Number 1, Call "*" [Number 2, Number 3]])
    it "Operators precedence === and +" $ "1+2===3" `shouldParseTo` (Call "===" [Call "+" [Number 1, Number 2],Number 3])
    it "Operators precedence === and =" $ "x=2===3" `shouldParseTo`  (Assign "x"( Call "===" [Number 2,Number 3]))

  describe "Associativity of operations" $ do   
    it "Add operation left" $ "1+2+3" `shouldParseTo` (Call "+" [Call "+" [Number 1,Number 2],Number 3])
    it "Minus operation left" $ "1-2-3" `shouldParseTo` (Call "-" [Call "-" [Number 1,Number 2],Number 3])
    it "Multiplication operation left" $ "1*2*3" `shouldParseTo` (Call "*" [Call "*" [Number 1,Number 2],Number 3])
    it "Modulo operation left" $ "1%2%3" `shouldParseTo` (Call "%" [Call "%" [Number 1,Number 2],Number 3])
    it "Less than operation left" $ "1<2<3" `shouldParseTo` (Call "<" [Call "<" [Number 1,Number 2],Number 3])
    it "Compare than operation left" $ "1===2===3" `shouldParseTo` (Call "===" [Call "===" [Number 1,Number 2],Number 3])
    it "Comma operation right" $ "1,2,3" `shouldParseTo` (Comma (Number 1) (Comma (Number 2) (Number 3)))
    it "Equal operation right" $ "x=y=z" `shouldParseTo` (Assign "x" (Assign "y" (Var "z")))
 
  describe "Complex expressions" $ do
    it "scope.js - provided file" $ "x = 42,y = [for (x of 'abc') x],[x, y]" `shouldParseTo` (Comma(Assign"x"(Number 42))(Comma(Assign"y"(Compr(ACFor"x"(String"abc")(ACBody(Var"x")))))(Array[Var"x",Var"y"])))
    it "intro.js - provided file" $ "xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],squares = [ for (x of xs) x * x ],evens = [ for (x of xs) if (x % 2 === 0) x ],many_a = [ for (x of xs) for (y of xs) 'a' ],hundred = [ for (i of [0])            for (x of Array(5))            for (y of Array(20)) i = i + 1 ],[xs, squares, evens, many_a, hundred]" `shouldParseTo` (Comma(Assign"xs"(Array[Number 0,Number 1,Number 2,Number 3,Number 4,Number 5,Number 6,Number 7,Number 8,Number 9]))(Comma(Assign"squares"(Compr(ACFor"x"(Var"xs")(ACBody(Call"*"[Var"x",Var"x"])))))(Comma(Assign"evens"(Compr(ACFor"x"(Var"xs")(ACIf(Call"==="[Call"%"[Var"x",Number 2],Number 0])(ACBody(Var"x"))))))(Comma(Assign"many_a"(Compr(ACFor"x"(Var"xs")(ACFor"y"(Var"xs")(ACBody(String"a"))))))(Comma(Assign"hundred"(Compr(ACFor"i"(Array[Number 0])(ACFor"x"(Call"Array"[Number 5])(ACFor"y"(Call"Array"[Number 20])(ACBody(Assign"i"(Call"+"[Var"i",Number 1]))))))))(Array[Var"xs",Var"squares",Var"evens",    Var"many_a",Var"hundred"]))))))