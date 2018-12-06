module Day5.Part2 where

import           Data.Char
import qualified Data.Set  as S

parse :: String -> String
parse = go []

go [] (y:ys) = go [y] ys
go acc [] = reverse $ tail acc
go acc@(a:as) (y:ys)
  | y `reactsWith` a = go as ys
  | otherwise = go (y : acc) ys

reactsWith x y = (isUpper x && toLower x == y) || (isLower x && toUpper x == y)

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
