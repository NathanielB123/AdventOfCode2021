{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ ( r )

type Pos = (Int, Int)
type Vel = (Int, Int)

day17p1 :: Range2 -> Int
day17p1 r
  = maximum $ map (maxHeight . snd) $ filter (`simulate` r) [(a, b)
  | a <- [0..500], b <- [0..500]]

day17p2 :: Range2 -> Int
day17p2 r@((_, maxX), (minY, _))
  = length $ filter (`simulate` r) [(a, b) | a <- [0..maxX], b <- [minY..1000]]


maxHeight :: Int -> Int
maxHeight x
  = x * (x+1) `div` 2

simulate :: Vel -> Range2 -> Bool
simulate
  = simulate' (0, 0)
  where 
    simulate' :: Pos -> Vel -> Range2 -> Bool
    simulate' (x, y) (vX, vY) r@(rX@(minX, maxX),rY@(minY, _))
      | x `between` rX && y `between` rY = True
      | y < minY && sign vY <= 0 
      || x < minX && sign vX <= 0 
      || x > maxX && sign vX >= 0 = False
      | otherwise = simulate' (x+vX, y+vY) (vX - sign vX, vY - 1) r

day17In :: String
day17In = [r|target area: x=20..30, y=-10..-5|]

day17InFull :: String
day17InFull = [r|target area: x=143..177, y=-106..-71|]

type Min = Int 
type Max = Int
type Range = (Min, Max)
type Range2 = (Range, Range)

day17In' :: Range2
day17In' 
  = (toRange $ ws !! 2, toRange $ ws !! 3)
  where
    ws = words day17InFull
    toRange :: String -> (Int, Int)
    toRange
      = first2 . map read . words . map (\c -> 
        if c `elem` ['.', ','] then ' ' else c) 
        . drop 2

sign :: Int -> Int
sign x
  | x > 0 = 1
  | x == 0 = 0
  | otherwise = -1

first2 :: [a] -> (a, a)
first2 (a: b: _)
  = (a, b)
first2 _
  = error "If Prelude is allowed to use partial functions instead of maybes, then so am I"

between :: Int -> Range -> Bool
between x (min, max)
  = min <= x && x <= max
