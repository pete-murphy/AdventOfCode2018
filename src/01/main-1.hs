module Main where

removePlus :: String -> String
removePlus = filter (/= '+')

solve :: String -> Int
solve = sum . map ((read :: String -> Int) . removePlus) . lines

main :: IO ()
main = do
  text <- readFile "input.txt"
  putStrLn $ show $ solve text
