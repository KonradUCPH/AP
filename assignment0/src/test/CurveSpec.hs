module CurveSpec (main, spec) where
    
    import Test.Hspec
    import Test.QuickCheck
    
    -- import module to test
    import Curves
    
    -- `main` is here so that this module can be run from GHCi on its own.  It is
    -- not needed for automatic spec discovery.
    main :: IO ()
    main = hspec spec

    -- defining reusable constants
    pointA = point (1,0)
    pointB = point (2,0)
    pointC = point (0,0)
    pointD = point (0,1)
    pointE = point (0,2)
    pointF = point (-2,-3)
    pointG = point (-1,5)
    pointH = point (2,3)

    curveA = curve pointA []
    curveB = curve pointB []
    curveC = curve pointC []
    curveAB = curve pointA [pointB]
    curveDE = curve pointD [pointE]
    curveFGH = curve pointF [pointG, pointH]
    bboxFGH = (point (-2,-3), point (2,5))

    
    -- Testcases
    spec :: Spec
    spec = do
      describe "Curve creation" $ do
        it "creating Curve with one Point" $ do
          toList (curve pointA []) `shouldBe` [pointA]
        it "creating Curve with multiple Points" $ do
          toList (curve pointA [pointA]) `shouldBe` [pointA, pointA]
      describe "Connecting Curves" $ do
        it "connecting two curves" $ do
          connect curveA curveB `shouldBe` curveAB
      describe "Translating Curve" $ do
        it "shifting curve starting at (0,0)" $ do
          translate curveC pointA `shouldBe` curveA
        it "shifting curve not starting at (0,0)" $ do
          translate curveA pointC `shouldBe` curveC
      describe "Rotating Curve" $ do
        it "rotate by 90 degrees" $ do
          rotate curveAB 90 `shouldBe` curveDE
        it "rotate by -90 degrees" $ do
          rotate curveDE (-90) `shouldBe` curveAB
      describe "Reflecting Curve" $ do
        it "reflect horizontally" $ do
          reflect curveAB (Horizontal 0) `shouldBe` curveAB
        it "reflect vertically" $ do
          reflect curveAB (Vertical 0) `shouldBe` rotate curveAB 180
      describe "Bounding Box of Curve" $ do
        it "Testing BB of complex curve" $ do
          bbox curveFGH `shouldBe` bboxFGH
      describe "Measurments of Curve" $ do
        it "width" $ do
          width curveFGH `shouldBe` 4
        it "height" $ do
          height curveFGH `shouldBe` 8
      describe "Normalize Curve" $ do
        it "normaize a complex curve" $ do
          normalize curveFGH `shouldBe` translate curveFGH pointC
      describe "Curve to List" $ do
        it "toList" $ do
          toList curveAB `shouldBe` [pointA, pointB]
      
