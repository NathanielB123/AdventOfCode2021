{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ ( r )
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State.Lazy (State, get, put, modify)

type Packet = [Bool]

hexCodes :: [(Char, [Bool])]
hexCodes = [('0', [False, False, False, False]),
  ('1', [False, False, False, True]),
  ('2', [False, False, True, False]),
  ('3', [False, False, True, True]),
  ('4', [False, True, False, False]),
  ('5', [False, True, False, True]),
  ('6', [False, True, True, False]),
  ('7', [False, True, True, True]),
  ('8', [True, False, False, False]),
  ('9', [True, False, False, True]),
  ('A', [True, False, True, False]),
  ('B', [True, False, True, True]),
  ('C', [True, True, False, False]),
  ('D', [True, True, False, True]),
  ('E', [True, True, True, False]),
  ('F', [True, True, True, True])]

hexToBin :: String -> [Bool]
hexToBin = concatMap (\x -> fromMaybe [] (lookup x hexCodes))

getVersion :: Packet -> Int
getVersion p
  = binToDec $ take 3 p

getType :: Packet -> Int
getType p
  = binToDec $ take 3 $ drop 3 p

getBody :: Packet -> [Bool]
getBody = drop 6

getLiteralVal :: Packet -> Int
-- Pre: p is a packet with type ID 4
getLiteralVal
  = binToDec . getLiteralVal' . getBody
  where
    getLiteralVal' :: [Bool] -> [Bool]
    getLiteralVal' (h:xs)
      | h = x ++ getLiteralVal' r
      | otherwise = x
      where
        (x, r) = splitAt 4 xs
    getLiteralVal' _
      = error "Unexpected number of bits encountered"

getSubPacketLen :: Packet -> Int
-- Pre: p is a packet with type ID 0
getSubPacketLen
  = binToDec . take 15 . getBody

getSubPacketNum :: Packet -> Int
-- Pre: p is a packet with type ID 1
getSubPacketNum
  = binToDec . take 11 . getBody

takeDrop :: Int -> Int -> [a] -> [a]
takeDrop x y l
  = take x $ drop y l

binToDec :: [Bool] -> Int
binToDec ds
  = binToDec' $ reverse ds
  where
    binToDec' []
      = 0
    binToDec' (d : ds)
      | d = 1 + 2 * binToDec' ds
      | otherwise = 2 * binToDec' ds

getPackets :: [Bool] -> Int
getPackets x 
  = v + getPackets x'
  where
    v = getVersion x
    t = getType x
    x' = drop 6 x

getPacketsState :: State [Bool] [Bool] -> State [Bool] [Bool]
getPacketsState x
  = do
    put (drop 3)

day16In :: String
day16In
  = [r|D2FE28|]

day16In' :: [Bool]
day16In'
  = hexToBin day16In

