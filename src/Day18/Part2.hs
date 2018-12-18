module Day18.Part2 where

solve :: String -> (Int, Int)
solve = undefined

main :: IO ()
main = do
  text <- readFile "src/Day18/input.txt"
  putStrLn $ show $ solve text
