module Curves (
    point,
    pointX,
    pointY,
    curve,
    connect,
    rotate,
    translate,
    Line (Horizontal, Vertical),
    reflect,
    bbox,
    width,
    height,
    toList,
    normalize,
    toSVG,
    toFile,
    Curve,
    Point,
    hilbert
    ) where

import Text.Printf

newtype Point = Point (Double, Double) -- Point in coordinate system
    deriving (Show)

instance Eq Point where
    (==) (Point (x1, y1)) (Point (x2, y2)) = abs ( x1 - x2 ) < 0.01
                                             &&  abs ( y1 - y2 ) < 0.01


-- bad typedef:
-- type Curve = [Point]
-- this would allow clients to create empty curves by passing [] as a
-- paramter instead of a curve

-- better typedef
newtype Curve = Curve [Point]
    deriving (Show, Eq)

point :: (Double, Double) -> Point
point (x, y) = Point (x, y)

pointX :: Point -> Double
pointX (Point (x,_)) = x

pointY :: Point -> Double
pointY (Point (_,y)) = y


-- creates a new curve with at least one point
-- to abselutly ensure that there are no empty courves, the curve
-- constructor needs to be hidden to the client
curve :: Point -> [Point] -> Curve
curve p ps = Curve (p:ps)

connect :: Curve -> Curve -> Curve
connect (Curve c1) (Curve c2) = Curve $ c1 ++ c2

-- rotates a curve by d degrees around (0,0)
rotate :: Curve -> Double -> Curve
rotate (Curve c) d = Curve $ map  (`rotatePoint` d) c

-- rotates a point by deg degrees around (0,0)
rotatePoint :: Point -> Double -> Point
rotatePoint (Point (x, y)) deg = let rad = deg2rad deg in
                                    Point (x * cos rad - y * sin rad,
                                            y * cos rad + x * sin rad)

deg2rad :: Double -> Double
deg2rad deg = deg * pi / 180

translate :: Curve -> Point -> Curve
translate (Curve []) _ = Curve [] {- this case should never happen, as a
curve always has at least one point, as ensured by the exportet constructor
function. This line keeps the compiler happy :) -}
translate (Curve(c:cs)) p = let shiftX = pointX p - pointX c
                                shiftY = pointY p - pointY c in
                    translate' (Curve (c:cs)) $ point (shiftX, shiftY)

-- translate curve by the the vector from (0,0) to the point
translate' :: Curve -> Point -> Curve
translate' c p = let shiftX = pointX p
                     shiftY = pointY p
                     ps = toList c
                    in
                    Curve $ map (\(Point(x, y)) ->
                            Point(x + shiftX, y + shiftY) ) ps

data Line = Vertical Double | Horizontal Double

reflect :: Curve -> Line -> Curve
reflect (Curve c) (Vertical d) =  Curve $ map  (\(Point(x1, y1)) ->
                                        Point(2 * (d - x1) + x1, y1))  c
reflect (Curve c) (Horizontal d) =  Curve $ map  (\(Point(x1, y1)) ->
                                        Point(x1, 2 * (d - y1) + y1))  c

bbox :: Curve -> (Point, Point)
bbox (Curve []) = (point (0,0), point (0,0)) -- keep the compiler happy
bbox (Curve (c:cs)) = foldl findMixAndMax (c,c) cs

findMixAndMax :: (Point, Point) -> Point -> (Point, Point)
findMixAndMax (pbl, ptr) p = let xbl = pointX pbl
                                 ybl = pointY pbl
                                 xtr = pointX ptr
                                 ytr = pointY ptr
                                 x   = pointX p
                                 y   = pointY p
                                in
                                (Point(min xbl x, min ybl y),
                                    Point(max xtr x, max ytr y))

width :: Curve -> Double
width c = let (pointA, pointB) = bbox c
        in
            abs $ pointX pointA - pointX pointB

height :: Curve -> Double
height c = let (pointA, pointB) = bbox c
        in
            abs $ pointY pointA - pointY pointB

toList :: Curve -> [Point]
toList (Curve ps) = ps

normalize :: Curve -> Curve
normalize c = let (pointBL, _) = bbox c
                    -- invert the bpttom left point
                  pointMBL = Point (- pointX pointBL, - pointY pointBL)
            in
                translate' c pointMBL

toSVG :: Curve -> String
toSVG c = let cnorm = normalize c
                        in
                        svgHeader cnorm ++ svgBody cnorm ++ "</g>\n</svg>"


svgBody :: Curve -> String
svgBody (Curve []) = "" -- make the compiler happy
svgBody (Curve [_]) = ""
svgBody (Curve (p1:p2:ps)) = svgOneLine (p1, p2) ++ svgBody (Curve (p2:ps))

svgOneLine :: (Point, Point) -> String
{-
<line style="stroke-width: 2px; stroke: black; fill:white"
        x1="..." x2="..." y1="..." y2="..." />
-}
svgOneLine (Point(x1, y1),Point(x2, y2)) =
    "<line style=\"stroke-width: 2px; stroke: black; fill:white\" \n"
        ++ printf "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\"  /> \n"
        x1 x2 y1 y2

svgHeader :: Curve -> String
svgHeader c = "<svg xmlns=\"http://www.w3.org/2000/svg\"\n"
    ++ printf "width=\"%.2fpx\" height=\"%.2fpx\" version=\"1.1\">\n<g>\n"
    (width c) (height c)

-- call toFile curve "file.svg"
toFile :: Curve -> FilePath -> IO ()
toFile c p = writeFile p (toSVG c)

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
 where  w = width c
        h = height c
        p = 6

        ch = reflect c $ Vertical 0

        c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
        c1 = c `translate` point (w+p+w, h)
        c2 = c
        c3 = ch `rotate` 90 `translate` point (0, h+p)