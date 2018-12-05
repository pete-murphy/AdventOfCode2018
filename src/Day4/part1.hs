{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Day4.Part1 where

import           Control.Arrow
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Text.Parsec

type Parser = Parsec String ()

type Total = Int

type Minute = Int

type Freq = Int

type TimeCard = Map Minute Freq

newtype Guard = G
  { num :: Int
  }

data Action
  = Shift Guard
  | Sleep
  | Wake

parseLine :: String -> (Minute, Action)
parseLine = undefined

solve :: String -> Int
solve = undefined

main :: IO ()
main = undefined

maximumValBy :: (b -> b -> Ordering) -> Map a b -> (a, b)
maximumValBy c = maximumBy (c `on` snd) . M.toList

maximumVal :: Ord b => Map a b -> (a, b)
maximumVal = maximumValBy compare

freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (, 1)
