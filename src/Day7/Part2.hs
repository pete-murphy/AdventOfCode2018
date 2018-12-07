module Day7.Part2 where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Ord
import           Data.Set            (Set)
import qualified Data.Set            as S

type Elves = Int

type Step = Char

solve :: Elves -> Map Step [Step] -> Int
solve n ms = undefined

-- solveWith2 :: Map Step [Step] -> Int
-- solveWith2 ms = go 0 (,) (map (uncurry replicate) $ zip [1..] (S.toList $ allSteps ms))
--   where go secs (e0,e1) [] = secs
stepDurations :: [(Step, Int)]
stepDurations = zip ['A' .. 'Z'] [1 ..]

orderOfSteps :: Map Step [Step] -> [[Step]]
orderOfSteps ms = go [] (zip (S.toList $ allSteps ms) [1 ..])
  where
    go :: [[Step]] -> [(Step, Int)] -> [[Step]]
    go acc = undefined

type Completed = [Step]

type Current = (Maybe (Step, Int), Maybe (Step, Int))

runSteps :: [(Step, Int)] -> [(Maybe (Step, Int), Maybe (Step, Int))]
runSteps queue = go queue (Nothing, Nothing) [] []
  where
    go [] curr comp acc         = acc
    go (q:qs) (c1, c2) comp acc = error "I don't want to implement this"

canBeDone :: Map Step [Step] -> [Step] -> Step -> Bool
canBeDone steps completed s =
  case steps M.!? s of
    Nothing -> True
    Just cs -> all (`elem` completed) cs

allSteps :: Map Step [Step] -> Set Step
allSteps = S.fromList . foldMap (uncurry (++) . ((: []) *** id)) . M.toList

parse :: String -> Map Step [Step]
parse = M.fromListWith (++) . map parseLine . lines
  where
    parseLine :: String -> (Char, String)
    parseLine = select . words
    select (_:y:_:_:_:_:_:(x:_):_) = (x, y)

main :: IO ()
main = do
  text <- readFile "src/Day7/input.txt"
  putStrLn $ show $ solve 5 $ parse text
