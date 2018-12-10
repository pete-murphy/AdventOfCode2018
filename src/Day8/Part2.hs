module Day8.Part2 where

import           Debug.Trace

data Node = Node
  { metadata :: [Int]
  , children :: [Node]
  } deriving (Eq, Ord, Show)

mkTree :: [Int] -> ([Int], Node)
mkTree (n:m:xs) = (xs'', Node ms cs)
  where
    (xs', cs) = mkChildren n (xs, [])
    (ms, xs'') = splitAt m xs'
    mkChildren :: Int -> ([Int], [Node]) -> ([Int], [Node])
    mkChildren n' (ys, acc)
      | n' == 0 = (ys, reverse acc)
      | otherwise = mkChildren (n' - 1) (ys', (c : acc))
      where
        (ys', c) = mkTree ys

parse :: String -> [Int]
parse = map read . words

solve :: [Int] -> Int
solve = foldTree . snd . mkTree

foldTree :: Node -> Int
foldTree (Node ms cs)
  | cs == [] = sum ms
  | otherwise = sum $ foldTree <$> map ((!!) cs') ((subtract 1) <$> ms)
  where
    cs' = cs ++ repeat empty
    empty :: Node
    empty = Node [] []

main :: IO ()
main = do
  text <- readFile "src/Day8/input.txt"
  putStrLn $ show $ solve $ parse text
