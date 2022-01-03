{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ ( r )
import Data.Functor ( (<&>) )
import Text.ParserCombinators.ReadP as P ( skipSpaces )
import Text.Read (readPrec_to_S, minPrec, Read (readPrec), lift)
import Data.Set (toList, fromList)
import Control.Monad (liftM2)

type W = Int
type X = Int
type Y = Int
type Z = Int
{-
day24Search :: [(String, Char, Either Int Char)] -> Int
day24Search l
  -- Using head and iterating in reverse order so can short circuit
  = head (filter (\x -> 0 `notElem` digits x && day24p1 l x == 0) [99999999999999,99999999999998..11111111111111])

run :: Int -> Int
run = day24p1 day24In'

run2 :: Int -> Int
run2 x = day24p1 day24In' (99999999999999 - x)
-}
day24p1 :: [(String, Char, Either Int Char)] -> Int -> (W, X, Y, Z)
day24p1 l x
  = day24p1' l (digits x) (0, 0, 0, 0)

day24p1' :: [(String, Char, Either Int Char)] -> [Int] -> (W, X, Y, Z) -> (W, X, Y, Z)
day24p1' ((i, a, b):r) ns (w, x, y, z)
  = day24p1' r ns' vars
  where
    (n:nt) = ns
    get :: Char -> Int
    get c
      | c == 'w' = w
      | c == 'x' = x
      | c == 'y' = y
      | c == 'z' = z
      | otherwise = error "Unexpected variable!"
    a' = get a
    b' = case b of
      Left n -> n
      Right c -> get c
    new
      | i == "inp" = n
      | i == "add" = a' + b'
      | i == "sub" = a' - b'
      | i == "mul" = a' * b'
      | i == "div" = a' `div` b'
      | i == "mod" = a' `mod` b'
      | i == "eql" = fromEnum $ a' == b'
      | otherwise = error "Unexpected instruction!"
    vars
      | a == 'w' = (new, x, y, z)
      | a == 'x' = (w, new, y, z)
      | a == 'y' = (w, x, new, z)
      | a == 'z' = (w, x, y, new)
      | otherwise = error "Unexpected variable!"
    ns'
      | i == "inp" = nt
      | otherwise = ns
day24p1' [] _ rs
  = rs

digits :: Int -> [Int]
digits
  = reverse . digits'
  where
    digits' :: Int -> [Int]
    digits' 0 = []
    digits' x = x `mod` 10 : digits' (x `div` 10)

findConsts :: [(String, Char, Either Int Char)] -> ([Int],[Int],[Int],[Int])
findConsts (i:is)
  = (ws, xs, ys, zs)
  where
    rs = map (day24p1 (i : takeWhile notInp is)) [1..9]
    ws = remDups $ map (\(w, _, _, _) -> w) rs
    xs = remDups $ map (\(_, x, _, _) -> x) rs
    ys = remDups $ map (\(_, _, y, _) -> y) rs
    zs = remDups $ map (\(_, _, _, z) -> z) rs
findConsts []
  = error "No instructions!"

findConstsAlt :: [(String, Char, Either Int Char)] -> [([Int], (Int, Int, Int, Int))]
findConstsAlt is
  = remDups $ map (\x -> (x, day24p1' (chunk is 0 ++ chunk is 1) x (0, 0, 0, 0))) (liftM2 (\x y -> [x, y]) [1..9] [1..9])

notInp :: (String, Char, Either Int Char) -> Bool
notInp (i, _, _) = i /= "inp"

chunk :: [(String, Char, Either Int Char)] -> Int -> [(String, Char, Either Int Char)]
chunk (i : is) x
  | x == 0 = i : takeWhile notInp is
  | otherwise = chunk (dropWhile notInp is) (x-1)
chunk [] _
  = error "No instructions!"

remDups :: Ord a => [a] -> [a]
remDups = toList . fromList


-- To simplify a lot, will need to alter end state of chunks - there must be a
-- better way...

-- I think I need to code up a proper compiler/automatic simplifier, that
-- keeps track of which variables are dependant on what and cancels down using
-- pre-defined rules

-- Will need to use some wacky data structures, so will prob be much
-- easier in python

{-
As using integers, should be able to in this case keep track of the possible 
numbers as I run through the program and use those to simplify 

Quick note,  if real numbers were instead used, this would become super 
interesting - would have to consider ranges - if I have [2,6] and then I apply 
mod 5, I now have [0, 1] and [2, 5).
If I then apply 1/, I now have (1/5, 1/2] and [1,infinity]

I believe the only way to do this in general is to define functions in a richer
way than just what happens to a single x - I am not sure it is possible to
extract this behaviour in general for functions defined on reals in finite time.
-}

day24InSimplified :: String
day24InSimplified = [r|inp w
add x 1
add y w
add y 14
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 6
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 6
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 13
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -12
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 7
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 12
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -2
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -9
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 7
mul y x
add z y|]

day24In :: String
day24In = [r|inp w
mul x 0
add x z
mod x 26
div z 1
add x 11
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 14
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 6
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 6
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 13
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -12
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 7
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 12
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -2
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -9
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 7
mul y x
add z y|]

day24In' :: [(String, Char, Either Int Char)]
day24In'
  = map (\l -> let x: y: z: _ = (words l ++ ["0"]) in (x, head y,
  swapEither (readEither z) <&> head)) $ lines day24InSimplified

swapEither :: Either a1 a2 -> Either a2 a1
swapEither = either Right Left

-- Modified readEither from Text.Read
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    _   -> Left s
 where
  read' =
    do
      x <- readPrec
      lift P.skipSpaces
      return x