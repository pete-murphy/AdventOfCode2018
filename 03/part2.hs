{-# LANGUAGE OverloadedStrings #-}

module Day3.Part2 where

import           Control.Applicative
import           Data.Char           (isDigit)
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes, listToMaybe, maybeToList)
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Text.Trifecta

sample = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

type Coord = (Int, Int)

parserLine :: Parser (Int, Coord, Int, Int)
parserLine = do
  _ <- char '#'
  n <- many digit
  _ <- many (noneOf "@") >> char '@' >> char ' '
  x <- many digit
  y <- char ',' >> many digit
  w <- string ": " >> many digit
  h <- char 'x' >> many digit
  return $ (read n, (read x, read y), read w, read h)

fromSuccess :: Result a -> a
fromSuccess (Success x) = x

generateCoords :: (Coord, Int, Int) -> [Coord]
generateCoords ((x, y), w, h) = do
  x' <- [x .. (x + w - 1)]
  y' <- [y .. (y + h - 1)]
  return $ (x', y')

duplicateCoords :: [String] -> Set Coord
duplicateCoords ls =
  S.fromList $ solve' $ map ((\(_, cs, w, h) -> (cs, w, h)) . parseLine) ls

solve' :: [(Coord, Int, Int)] -> [Coord]
solve' xs =
  map head $
  filter (\g -> length g > 1) $ group $ sort $ concatMap generateCoords xs

parseLine :: String -> (Int, Coord, Int, Int)
parseLine = fromSuccess . parseString parserLine mempty

overlaps :: (Int, Coord, Int, Int) -> Set Coord -> Maybe Int
overlaps (n, cs, w, h) dups =
  case ((all .) (not .) <$> flip S.member) dups (generateCoords (cs, w, h)) of
    True  -> pure n
    False -> Nothing

main :: IO ()
main = do
  text <- lines <$> readFile "input.txt"
  putStrLn $
    show $
    listToMaybe $
    catMaybes $ map (((flip overlaps) (duplicateCoords text)) . parseLine) text
