module Day1.Part2 where

import           Control.Monad
import           Criterion.Main
import           Data.Function  (fix)
import           Data.Maybe     (listToMaybe)
import qualified Data.Set       as Set

removePlus :: String -> String
removePlus = filter (/= '+')

toInts :: String -> [Int]
toInts = map (read . removePlus) . lines

solve_ :: [Int] -> Int
solve_ xs = go [0] xs
  where
    go acc [] = go acc xs
    go acc@(a:_) (y:ys) =
      if elem (y + a) acc
        then (y + a)
        else go ((y + a) : acc) ys

solveWithSet :: [Int] -> Int
solveWithSet xs = go 0 (Set.singleton 0) (cycle xs)
  where
    go acc set (y:ys) =
      let y' = y + acc
       in if Set.member y' set
            then y'
            else go y' (Set.insert y' set) ys

solveWithScan :: [Int] -> Maybe Int
solveWithScan xs =
  listToMaybe $
  fst $
  foldl
    (\(accList, accSet) x ->
       if Set.member x accSet
         then ((x : accList), accSet)
         else (accList, Set.insert x accSet))
    ([], Set.singleton 0) $
  scanl (+) 0 (cycle xs)

solve2 :: [Int] -> Maybe Int
solve2 xs =
  fix (\r f a -> either id (r f) (f a)) coalg (cycle xs, 0, Set.singleton 0)
  where
    coalg ([], _, _) = Left Nothing
    coalg ((x:xs), acc, s)
      | Set.member acc' s = Left $ Just acc'
      | otherwise = Right (xs, acc', Set.insert acc' s)
      where
        acc' = x + acc

solve :: String -> Int
solve = solveWithSet . toInts

main' :: IO ()
main' = do
  listOfInts <- toInts <$> readFile "input-1.txt"
  putStrLn $ show $ solveWithSet listOfInts

main :: IO ()
main =
  defaultMain
    [ bench "solve_ with go" $
      nfIO $ solveWithSet . toInts <$> readFile "input-1.txt"
    , bench "solve_ with fix" $
      nfIO $ solve2 . toInts <$> readFile "input-1.txt"
    ]
