{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Day5.Part2 where

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

parse :: String -> String
parse (x:xs) = go [x] xs
  where
    go [] (y:ys)
      | otherwise = go (y : []) ys
    go acc [] = reverse $ tail acc
    go acc@(a:as) rest@(y:ys)
      | y `reactsWith` a = go as ys
      | otherwise = go (y : acc) ys
    reactsWith x y =
      (isUpper x && toLower x == y) || (isLower x && toUpper x == y)

solve :: String -> Int
solve =
  S.findMin . S.fromList . map (length . parse) . zipWith f alphaPairs . repeat
  where
    f xs = filter (`notElem` xs)
    alphaPairs = zipWith ((:)) ['a' .. 'z'] (map (: []) ['A' .. 'Z'])

main :: IO ()
main = do
  text <- readFile "src/Day5/input.txt"
  putStrLn $ show $ solve text
