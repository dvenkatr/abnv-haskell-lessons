module Slope where
-- type Point = (x, y)
data Point = Point {x :: Int, y :: Int}
data Line = Line Point Point

slope :: Point -> Point -> Double
slope (Point x1 y1) (Point x2 y2) = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)

data Slope = Slope Double | YAxis deriving Show

slope' :: Line -> Slope
slope' (Line (Point x1 y1) (Point x2 y2)) = if x2 == x1
  then YAxis
  else Slope (fromIntegral (y2 - y1) / fromIntegral (x2 - x1))

-- *Slope> let l = Line (Point 2 3) (Point 4 5)
-- *Slope> slope' l
-- 1.0
-- *Slope> let l' = Line (Point 4 5) (Point 4 7)
-- *Slope> slope' l'
-- YAxis
