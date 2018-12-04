{-# LANGUAGE OverloadedStrings #-}

module Day3.Part2 where

import           Data.Char   (isDigit)
import           Data.List
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Maybe  (catMaybes)
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Text.Parsec

type Coord = (Int, Int)

type Parser = Parsec String ()

lineParser :: Parser (Int, Coord, Int, Int)
lineParser = do
  n <- char '#' >> many digit
  x <- string " @ " >> many digit
  y <- char ',' >> many digit
  w <- string ": " >> many digit
  h <- char 'x' >> many digit
  return $ (read n, (read x, read y), read w, read h)

parseLine :: String -> (Int, Coord, Int, Int)
parseLine = fromRight . parse lineParser ""

fromRight :: Either b a -> a
fromRight (Right x) = x

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
    head $
    catMaybes $ map (((flip overlaps) (duplicateCoords text)) . parseLine) text
