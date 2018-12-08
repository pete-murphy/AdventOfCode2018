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
import           Debug.Trace

type Elves = Int

type Step = Char

solve :: Elves -> Map Step [Step] -> Int
solve n ms = runSteps n ms

solve_ :: Elves -> Map Step [Step] -> Int
solve_ n ms = runSteps_ n ms

runSteps_ :: Elves -> Map Step [Step] -> Int
runSteps_ n input = go (stepDurations_ 0) M.empty [] (0)
  where
    go ::
         Map Step Int {- ^-- Queue -}
      -> Map Step Int {- ^-- Current steps with time remaining per -}
      -> [Step] {- ^-- Completed steps (to reference when finding next available step) -}
      -> Int {- ^-- Current time -}
      -> Int {- ^-- Total time -}
    go qs curr comp acc
      | M.size qs == 0 = trace (show $ M.foldr (+) acc curr) acc
      | M.size curr < n =
        case findNext qs comp input of
          Just k ->
            go (M.delete (fst k) qs) (uncurry M.insert k curr) comp (acc)
          Nothing ->
            case M.partition (== 0) (M.map (subtract 1) curr) {- ^-- If all possible threads are busy, tick forward, marking 0 as complete -}
                  of
              (x, y)
                | x == M.empty -> go qs y comp (acc + 1)
                | otherwise -> go qs y (M.keys x ++ comp) (acc + 1)
      | otherwise =
        case M.partition (== 0) (M.map (subtract 1) curr) {- ^-- If all possible threads are busy, tick forward, marking 0 as complete -}
              of
          (x, y)
            | x == M.empty -> trace (show $ M.toList y) go qs y comp (acc + 1)
            | otherwise ->
              trace (show $ M.toList y) go qs y (M.keys x ++ comp) (acc + 1)

stepDurations_ :: Int -> Map Step Int
stepDurations_ n = M.fromList $ zip ['A' .. 'F'] [n + 1 ..]

stepDurations :: Int -> Map Step Int
stepDurations n = M.fromList $ zip ['A' .. 'Z'] [n + 1 ..]

getStepDurations :: Map Step [Step] -> [(Step, Int)]
getStepDurations = map (id &&& ((M.!) $ stepDurations 0)) . map fst . M.toList

runSteps :: Elves -> Map Step [Step] -> Int
runSteps n input = go (stepDurations 60) M.empty [] (0)
  where
    go ::
         Map Step Int {- ^-- Queue -}
      -> Map Step Int {- ^-- Current steps with time remaining per -}
      -> [Step] {- ^-- Completed steps (to reference when finding next available step) -}
      -> Int {- ^-- Current time -}
      -> Int {- ^-- Total time -}
    go qs curr comp acc
      | M.size qs == 0 = M.foldr (+) acc curr {- Holy shite! This line cost me my sanity -}
      | M.size curr < n =
        case findNext qs comp input of
          Just k ->
            go (M.delete (fst k) qs) (uncurry M.insert k curr) comp (acc)
          Nothing ->
            case M.partition (== 0) (M.map (subtract 1) curr) {- ^-- If all possible threads are busy, tick forward, marking 0 as complete -}
                  of
              (x, y)
                | x == M.empty -> go qs y comp (acc + 1)
                | otherwise -> go qs y (M.keys x ++ comp) (acc + 1)
      | otherwise =
        case M.partition (== 0) (M.map (subtract 1) curr) {- ^-- If all possible threads are busy, tick forward, marking 0 as complete -}
              of
          (x, y)
            | x == M.empty -> trace (show $ M.toList y) go qs y comp (acc + 1)
            | otherwise ->
              trace (show $ M.toList y) go qs y (M.keys x ++ comp) (acc + 1)

findNext :: Map Step Int -> [Step] -> Map Step [Step] -> Maybe (Step, Int)
findNext queue completed charsAndDeps =
  find ((canBeAssigned charsAndDeps completed) . fst) $ (M.toList queue)

ms :: Map Step [Step]
ms = parse sample

ss :: [Step]
ss = S.toList $ allSteps ms

qs :: Map Step Int
qs = M.fromList $ getStepDurations ms

canBeAssigned :: Map Step [Step] -> [Step] -> Step -> Bool
canBeAssigned steps completed s =
  case M.lookup s steps of
    Nothing -> True
    Just cs -> all (`elem` completed) cs

allSteps :: Map Step [Step] -> Set Step
allSteps = S.fromList . foldMap (uncurry (++) . ((: []) *** id)) . M.toList

parse :: String -> Map Step [Step]
parse = M.map sort . M.fromListWith (++) . map parseLine . lines
  where
    parseLine :: String -> (Char, String)
    parseLine = select . words
    select (_:y:_:_:_:_:_:(x:_):_) = (x, y)

main :: IO ()
main = do
  text <- readFile "src/Day7/input.txt"
  putStrLn $ show $ solve 5 $ parse text

main_ :: IO ()
main_ = do
  text <- readFile "src/Day7/sample.txt"
  putStrLn $ show $ solve_ 2 $ parse text

sample :: String
sample =
  intercalate
    "\n"
    [ "Step C must be finished before step A can begin."
    , "Step C must be finished before step F can begin."
    , "Step A must be finished before step B can begin."
    , "Step A must be finished before step D can begin."
    , "Step B must be finished before step E can begin."
    , "Step D must be finished before step E can begin."
    , "Step F must be finished before step E can begin."
    ]
