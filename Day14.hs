{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ ( r )
import Data.List (sort)

day14 :: (String, [(String, Char)]) -> Int
day14 (c, rs)
  = map getVal (iterate (`day14'` rs) c) !! 10
  where
    day14' :: String -> [(String, Char)] -> String
    day14' (x1 : x2 : xs) rs
      = case i of
        Just i' -> x1 : i' : day14' (x2 : xs) rs
        Nothing -> x1 : day14' (x2 : xs) rs
      where
        px = x1 : [x2]
        i = lookup px rs
    day14' xs _
      = xs

-- Using a much faster method
day14p2 :: (String, [(String, (String, String))]) -> Int
day14p2 (c, rs)
  = map (\x -> let lc = last c  in getValB $ ([lc, lc], 1) : x) (iterate (`day14'` rs) (foldr countUp [] $ getPairs c)) !! 40
  where
    day14' :: [(String, Int)] -> [(String, (String, String))] -> [(String, Int)]
    day14' xs rs
      = foldr (\(x, c) acc -> insertAndCount (x, c) acc) [] xs
      where
        insertAndCount :: (String, Int) -> [(String, Int)] -> [(String, Int)]
        insertAndCount (x, c) = countUpNew (case lookup x rs of
          Just (a, b) -> [(a, c), (b, c)]
          Nothing -> [(x, c)])

getPairs :: String -> [String]
getPairs (x1 : xs@(x2 : _))
  = [x1, x2] : getPairs xs
getPairs _
  = []

getVal :: String -> Int
getVal xs
  = last final - head final
  where
    final = sort $ map snd $ foldr countUp [] xs

getValB :: [(String, Int)] -> Int
getValB xs
  = last final - head final
  where
    final = sort $ map snd $ foldr (\(x, c) -> countUp2 (head x) c) [] xs

countUp :: Eq a => a -> [(a, Int)] -> [(a, Int)]
countUp x cs
  = case c of
    Just (c', i') -> take i' cs ++ (x, c' + 1) : drop (i' + 1) cs
    Nothing -> (x, 1) : cs
  where
    c = lookup x $ zipWith (\(x, c) i -> (x, (c, i))) cs [0..]

countUp2 :: Eq a => a -> Int -> [(a, Int)] -> [(a, Int)]
countUp2 x c cs
  = case found of
    Just (c', i') -> take i' cs ++ (x, c' + c) : drop (i' + 1) cs
    Nothing -> (x, c) : cs
  where
    found = lookup x $ zipWith (\(x, c) i -> (x, (c, i))) cs [0..]

countUpNew :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
countUpNew ((x, c) : xs) acc
  = countUpNew xs acc'
  where
    found = lookup x $ zipWith (\(x, c) i -> (x, (c, i))) acc [0..]
    acc' = case found of
      Just (c', i') -> take i' acc ++ (x, c' + c) : drop (i' + 1) acc
      Nothing -> (x, c) : acc
countUpNew [] acc
  = acc

day14In :: String
day14In = [r|NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C|]

day14InFull :: String
day14InFull = [r|OFSNKKHCBSNKBKFFCVNB

KC -> F
CO -> S
FH -> K
VP -> P
KF -> S
SV -> O
CB -> H
PN -> F
NC -> N
BC -> F
NP -> O
SK -> F
HS -> C
SN -> V
OP -> F
ON -> N
FK -> N
SH -> B
HN -> N
BO -> V
VK -> H
SC -> K
KP -> O
VO -> V
HC -> P
BK -> B
VH -> N
PV -> O
HB -> H
VS -> F
KK -> B
HH -> B
CF -> F
PH -> C
NS -> V
SO -> P
NV -> K
BP -> N
SF -> V
SS -> K
FP -> N
PC -> S
OH -> B
CH -> H
VV -> S
VN -> O
OB -> K
PF -> H
CS -> C
PP -> O
NF -> H
SP -> P
OS -> V
BB -> P
NO -> F
VB -> V
HK -> C
NK -> O
HP -> B
HV -> V
BF -> V
KO -> F
BV -> H
KV -> B
OF -> V
NB -> F
VF -> C
PB -> B
FF -> H
CP -> C
KH -> H
NH -> P
PS -> P
PK -> P
CC -> K
BS -> V
SB -> K
OO -> B
OK -> F
BH -> B
CV -> F
FN -> V
CN -> P
KB -> B
FO -> H
PO -> S
HO -> H
CK -> B
KN -> C
FS -> K
OC -> P
FV -> N
OV -> K
BN -> H
HF -> V
VC -> S
FB -> S
NN -> P
FC -> B
KS -> N|]

day14In' :: (String, [(String, Char)])
day14In'
  = (head ls, map (\x -> (take 2 x, x !! 6)) $ drop 2 ls)
  where
    ls = lines day14InFull
day14In'' :: (String, [(String, (String, String))])
day14In''
  = (head ls, map (\x -> let x' = take 2 x in (x', ([head x', x !! 6], x !! 6 : tail x'))) $ drop 2 ls)
  where
    ls = lines day14InFull
