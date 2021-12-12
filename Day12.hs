{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ ( r )
import Data.Char ( toUpper )

day12p1 :: [(String, String)] -> Int
day12p1 ls
  = day12p1' ls "start" "end" ["start"]
  where
    day12p1' :: [(String, String)] -> String -> String -> [String] -> Int
    day12p1' ls o d vs
      | o == d = 1
      | otherwise = sum $ map (\x -> day12p1' ls x d $ if isUpper x then vs else
          x : vs) adj
      where
        adj = map snd $ filter (\(x, y) -> x == o && notElem y vs) ls

day12p2 :: [(String, String)] -> Int
day12p2 ls
  = day12p2' ls "start" "end" ["start"] False
  where
    day12p2' :: [(String, String)] -> String -> String -> [String] -> Bool 
      -> Int
    day12p2' ls o d vs smallVisit
      | o == d = 1
      | otherwise = sum $ map (\x -> day12p2' ls x d (if isUpper x then vs else 
          x : vs) $ smallVisit || (x `elem` vs && not (isUpper x))) adj
      where
        adj = map snd $ filter (\(x, y) -> x == o && (y `notElem` vs || not 
          smallVisit) && y /= "start") ls

isUpper :: String -> Bool
isUpper x
  = map toUpper x == x

day12In :: String
day12In = [r|kc-qy
qy-FN
kc-ZP
end-FN
li-ZP
yc-start
end-qy
yc-ZP
wx-ZP
qy-li
yc-li
yc-wx
kc-FN
FN-li
li-wx
kc-wx
ZP-start
li-kc
qy-nv
ZP-qy
nv-xr
wx-start
end-nv
kc-nv
nv-XQ|]

day12In' :: [(String, String)]
day12In'
  = half ++ map (\(x, y) -> (y, x)) half
  where
    half = map (\x -> read $ "(\"" ++  (x >>= (\c -> if c == '-' then "\",\"" else [c]))
      ++ "\")") $ lines day12In