module Day2.Part1 where

import           Data.List

twos :: String -> [String]
twos str = filter (\xs -> length xs == 2) $ group $ sort str

threes :: String -> [String]
threes str = filter (\xs -> length xs == 3) $ group $ sort str

solve :: String -> Int
solve xs = (foo x) * (foo y)
  where
    foo :: [[String]] -> Int
    foo xss = sum $ map goo xss
      where
        goo [] = 0
        goo _  = 1
    xs' = lines xs
    x = twos <$> xs'
    y = threes <$> xs'

main :: IO ()
main = do
  text <- readFile "input.txt"
  putStrLn $ show $ solve text
