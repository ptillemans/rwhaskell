module Ch3Exercises where

import Data.List (sortBy, minimumBy)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

average' :: Fractional a => [a] -> a
average' xs = s / l
  where
    s = sum xs
    l = fromIntegral $ length xs

palindrome :: [a] -> [a]
palindrome xs = xs ++ rev
  where rev = reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == rev
  where rev = reverse xs

-- sortByLength :: Foldable f => [f a] -> [f a]
sortByLength xs = sortBy compareLength xs
  where compareLength a b = compare (length a) (length b)

intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = x
intersperse' a (x:xs) = x ++ [a] ++ intersperse' a xs

data Tree a = Node a (Tree a) (Tree a)
            | Empty
           deriving (Show)


height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + maxHeight
  where leftHeight = height left
        rightHeight = height left
        maxHeight = max leftHeight rightHeight

-- exercises 9 ...

data Direction = LeftTurn | Straight | RightTurn
  deriving (Eq, Show)

data Point = Point {x::Float, y::Float}
  deriving (Show)

vectorAngle :: Point -> Point -> Float
vectorAngle o a = atan2 (y a - y o) (x a - x o)

turn :: Point -> Point -> Point -> Direction
turn a b c
  | angle > 0 = RightTurn
  | angle < 0 = LeftTurn
  | otherwise = Straight
  where
    angle1 = vectorAngle a b
    angle2 = vectorAngle c b
    angle = normalizeAngle $ angle2 - angle1
    normalizeAngle x
     | x < (-pi) = normalizeAngle (x + 2 * pi)
     | x > pi    = normalizeAngle (x - 2 * pi)
     | otherwise     = x

turns :: [Point] -> [Direction]
turns [] = []
turns [_] = []
turns [_, _] = []
turns (x:y:z:xs) = t : turns (y:z:xs)
  where t = turn x y z

convexHull :: [Point] -> [Point]
convexHull points =
  grahamScan ps ++ [p]
  where
    p = minimumBy (\p1 p2 -> compare (y p1) (y p2) ) points
    ps = sortBy (\p1 p2 -> compare (vectorAngle p p1) (vectorAngle p p2)) points

grahamScan :: [Point] -> [Point]
grahamScan [x, y] = [x, y]
grahamScan (x:y:z:xs)
  | t == RightTurn = grahamScan (x:z:xs)
  | otherwise      = x : grahamScan (y:z:xs)
  where t = turn x y z
