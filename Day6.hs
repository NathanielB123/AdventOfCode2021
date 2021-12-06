{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ

day6 :: [Int] -> Int -> Int
day6 ts n
  = length $ iterate step ts !! n
  where
    step ts
      = ts >>= perFish
    perFish t
      | t == 0 = [6, 8]
      | otherwise = [t-1]

day6Optimised :: [Int] -> Int -> Int
day6Optimised ts n
  = sum $ iterate step (foldl toIndex (replicate 9 0) ts) !! n
  where
    step (t : ts)
      = replace ts' 6 (ts' !! 6 + t)
      where
        ts' = ts ++ [t]
    step _
      = error "The list collapsed somehow"
    toIndex i t
      = replace i t (i !! t + 1)

replace :: [Int] -> Int -> Int -> [Int]
replace l i x
  = take i l ++ x : drop (i+1) l

day6In :: String
day6In = [r|3,4,3,1,2|]

day6InFull :: String
day6InFull = [r|1,3,3,4,5,1,1,1,1,1,1,2,1,4,1,1,1,5,2,2,4,3,1,1,2,5,4,2,2,3,1,2,3,2,1,1,4,4,2,4,4,1,2,4,3,3,3,1,1,3,4,5,2,5,1,2,5,1,1,1,3,2,3,3,1,4,1,1,4,1,4,1,1,1,1,5,4,2,1,2,2,5,5,1,1,1,1,2,1,1,1,1,3,2,3,1,4,3,1,1,3,1,1,1,1,3,3,4,5,1,1,5,4,4,4,4,2,5,1,1,2,5,1,3,4,4,1,4,1,5,5,2,4,5,1,1,3,1,3,1,4,1,3,1,2,2,1,5,1,5,1,3,1,3,1,4,1,4,5,1,4,5,1,1,5,2,2,4,5,1,3,2,4,2,1,1,1,2,1,2,1,3,4,4,2,2,4,2,1,4,1,3,1,3,5,3,1,1,2,2,1,5,2,1,1,1,1,1,5,4,3,5,3,3,1,5,5,4,4,2,1,1,1,2,5,3,3,2,1,1,1,5,5,3,1,4,4,2,4,2,1,1,1,5,1,2,4,1,3,4,4,2,1,4,2,1,3,4,3,3,2,3,1,5,3,1,1,5,1,2,2,4,4,1,2,3,1,2,1,1,2,1,1,1,2,3,5,5,1,2,3,1,3,5,4,2,1,3,3,4|]

day6In' :: [Int]
day6In' = read $ '[' : day6InFull ++ "]"
