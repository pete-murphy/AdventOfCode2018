{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Day4.Part1 where

import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Text.Parsec

type Parser = Parsec String ()

type Guard = Int

type Total = Int

type Minute = Int

type Freq = Int

data Shift =
  Shift Guard
        Total
        (Map Minute Freq)

parseGuard :: Parser Guard
parseGuard = undefined

parseShift :: Parser Shift
parseShift = undefined

solve :: String -> Int
solve = undefined

main :: IO ()
main = undefined
