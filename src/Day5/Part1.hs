{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Day5.Part1 where

import           Control.Arrow
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Text.Trifecta

example = "dabAcCaCBAcCcaDA"

parse :: String -> String
parse (x:xs) = go [x] xs
  where
    go [] (y:ys)
      | otherwise = go (y : []) ys
    go acc [] = reverse acc
    go acc@(a:as) rest@(y:ys)
      | y `reactsWith` a = go as ys
      | otherwise = go (y : acc) ys
    reactsWith x y =
      (isUpper x && toLower x == y) || (isLower x && toUpper x == y)

-- | Have to subtract 1 for '\n'
solve :: String -> Int
solve = subtract 1 . length . parse

main :: IO ()
main = do
  text <- readFile "src/Day5/input.txt"
  putStrLn $ show $ solve text
