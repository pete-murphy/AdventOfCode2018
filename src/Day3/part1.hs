{-# LANGUAGE OverloadedStrings #-}

module Day3.Part1 where

import           Control.Applicative
import           Data.Char           (isDigit)
import           Data.List
import           Text.Trifecta

sample = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

type Coord = (Int, Int)

parserLine :: Parser (Coord, Int, Int)
parserLine = do
  _ <- many (noneOf "@") >> char '@' >> char ' '
  x <- many digit
  y <- char ',' >> many digit
  w <- string ": " >> many digit
  h <- char 'x' >> many digit
  return $ ((read x, read y), read w, read h)

fromSuccess :: Result a -> a
fromSuccess (Success x) = x

generateCoords :: (Coord, Int, Int) -> [Coord]
generateCoords ((x, y), w, h) = do
  x' <- [x .. (x + w - 1)]
  y' <- [y .. (y + h - 1)]
  return $ (x', y')

solve' :: [(Coord, Int, Int)] -> Int
solve' xs =
  length $
  map head $
  filter (\g -> length g > 1) $ group $ sort $ concatMap generateCoords xs

solve = solve' . map parseLine . lines

parseLine :: String -> (Coord, Int, Int)
parseLine = fromSuccess . parseString parserLine mempty

main :: IO ()
main = do
  text <- lines <$> readFile "input.txt"
  putStrLn $ show $ solve' $ map parseLine text
