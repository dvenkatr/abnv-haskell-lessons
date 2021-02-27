-- Write a data constructor to represent shapes (circle, square, rectangle)

-- data Shape = Circle Int | Square Int | Rectangle Int Int

module Shape where

-- data Point = Point Int Int
data Point = Point { pointX :: Int, pointY :: Int} -- p = Point 1 2 or p = Point {pX =1, pY =2}

type Point' = (Int, Int) -- equivalent to type Z = (Int, Int) -- alias, no constructor
type String' = [Char]
type Age = Int
type Dist = Int -- can pass Age to function which takes Dist, so use newtype instead

newtype Point'' = Point''(Int, Int) -- have to use constructor, use for single constructor types -- p = Point''(1,2)
-- newtype NewPoint = NewPoint { newPointX :: Int, newPointY :: Int} -- doesn't work
newtype TeamId = TeamId {unTeamId :: Int}

data Shape = Circle Point Int
           | Square Point Int
           | Rectangle Point Int Int
           | EqTri Point Int

area :: Shape -> Double
area (Circle _ rad) = 3.14 * fromIntegral (rad * rad) --fromIntegral converts int to double
area (Square _ side) = fromIntegral (side * side)
area (Rectangle _ len width) = fromIntegral (len * width)
area _ = error "unknown" -- or _ = error "unknown"

area' s = case s of
  Circle _ignore rad -> 3 * rad * rad -- _nameofvariable -- this is a variable you don't use in this function
  Square _ side -> side * side
  Rectangle _ len width -> len * width
  _ -> error "unknown"

origin (Circle o _) = (pointX o, pointY o) -- pointX is a function on Point

-- person
data Person = Person { name :: [Char], age :: Int, loc :: Point }
