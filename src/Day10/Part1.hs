module Day10.Part1 where

import           Control.Arrow
import           Data.Char
import           Data.List.Split
import           Linear.V2

type Pos = V2 Int

type Vel = V2 Int

parse :: String -> [(Pos, Vel)]
parse = map parseLine . lines
  where
    parseLine = format . map read . filter (any isDigit) . splitOneOf "<,>"
    format (px:py:vx:vy:_) = (V2 px py, V2 vx vy)

run :: [(Pos, Vel)] -> [(Pos, Vel)]
run = map (\(p, v) -> (p + v, v))

run' :: [(Pos, Vel)] -> [(Pos, Vel)]
run' = map (uncurry (+) &&& snd)

main :: IO ()
main = do
  text <- readFile "src/Day10/sample.txt"
  mapM_ (putStrLn . show) $ (run &&& run') $ parse text
