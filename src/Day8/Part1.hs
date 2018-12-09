module Day8.Part1 where

{- `Node` is monomorphic, equivalent of `Tree [Int]` -}
data Node = Node
  { metadata :: [Int]
  , children :: [Node]
  }

mkTree :: [Int] -> ([Int], Node)
{- NB: When we *run* this, if the input list of `Int`s is well-formatted,
   the remaining, unconsumed list of `Int`s (the left side of the returned
   tuple) will be empty. -}
mkTree (n:m:xs) = (xs'', Node ms cs)
  {- `n` is number of child nodes, `m` is number of metadata elems,
     `xs''` is what remains after making `n` children and removing
     `m` metadata elements from `xs`. -}
  where
    (xs', cs) = mkChildren n (xs, [])
    {- We're going to make `n` child nodes out of `xs`,
       but we also need to get back the remainder of `xs`
       that we haven't consumed. `xs'` is what remains of
       `xs` after making child nodes, `cs` are our child
       nodes. -}
    (ms, xs'') = splitAt m xs'
    {- To get the metadata, we take our remainder `xs'`
       and take `m` elements (these are the metadata elems)
       and now we have `xs''` as our new remainder (unconsumed
       list). -}
    mkChildren :: Int -> ([Int], [Node]) -> ([Int], [Node])
    {- `mkChildren` takes an `Int` (number of child nodes to make)
       and a tuple of the list being consumed (`ys`) and
       accumulates a list of `Node`s. It returns the unconsumed
       remainder of the list and the accumulated children. -}
    mkChildren n' (ys, acc)
      | n' == 0 = (ys, acc)
      {- Base case: we have created `n'` children -}
      | otherwise = mkChildren (n' - 1) (ys', (c : acc))
      {- Otherwise, let's make another child 💏,
         and add it to our accumulator. -}
      where
        (ys', c) = mkTree ys {- We make a child node by calling `mkTree`
           on the remaining list. We get back a tuple
           of the new remaining list and the child node. -}

solve :: [Int] -> Int
solve = foldTree (\xs bs -> sum (sum xs : bs)) . snd . mkTree

foldTree :: ([Int] -> [Int] -> Int) -> Node -> Int
foldTree f = go
  where
    go (Node ms ts) = f ms (map go ts)

parse :: String -> [Int]
parse = map read . words

main :: IO ()
main = do
  text <- readFile "src/Day8/input.txt"
  putStrLn $ show $ solve $ parse text
