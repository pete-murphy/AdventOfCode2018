{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Day4.Part2 where

import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Set          (Set)
import qualified Data.Set          as S
import           Text.RawString.QQ

type EntryRow = ([Int], Entry)

type Shift = [EntryRow]

type GuardId = Int

type Minutes = Int

type Minute = Int

data Entry
  = Guard GuardId
  | Wake
  | Sleep
  deriving (Eq, Ord, Show)

parseLine :: String -> EntryRow
parseLine =
  ((parseTime . map (read) . drop 3 . words . justDigits) ***
   (parseEntry . drop 1)) .
  splitAt 18
  where
    parseTime :: [Int] -> [Int]
    parseTime [0, x] = [24, x]
    parseTime x      = x
    parseEntry :: String -> Entry
    parseEntry ('G':rest) =
      Guard (read $ takeWhile isDigit $ dropWhile (not . isDigit) rest)
    parseEntry ('w':_) = Wake
    parseEntry ('f':_) = Sleep

groupEntries :: [EntryRow] -> [Shift]
groupEntries [] = []
groupEntries (e@(_, Guard _):rest) =
  (e : takeWhile isSameShift rest) : groupEntries (dropWhile isSameShift rest)
  where
    isSameShift :: EntryRow -> Bool
    isSameShift (_, Guard _) = False
    isSameShift _            = True

toMapGuardFreqs :: [Shift] -> Map GuardId (Map Minute Int)
toMapGuardFreqs [] = M.empty
toMapGuardFreqs (((_, Guard n):shift):xs) =
  M.insertWith
    insertFn
    n
    (M.fromList $ map (, 1) $ getMinutesSleeping shift)
    (toMapGuardFreqs xs)
  where
    getMinutesSleeping :: Shift -> [Minute]
    getMinutesSleeping [] = []
    getMinutesSleeping (([_, m0], Sleep):([_, m1], Wake):xs) =
      [m0 .. m1 - 1] ++ getMinutesSleeping xs
    getMinutesSleeping (_:xs) = getMinutesSleeping xs
    insertFn :: Map Minute Int -> Map Minute Int -> Map Minute Int
    insertFn = M.unionWith (+)

toMapGuardMinutes :: [Shift] -> Map GuardId Minutes
toMapGuardMinutes [] = M.empty
toMapGuardMinutes (((_, Guard n):shift):xs) =
  M.insertWith (+) n (getTimeSleeping shift) (toMapGuardMinutes xs)
  where
    getTimeSleeping :: Shift -> Int
    getTimeSleeping [] = 0
    getTimeSleeping ((t0, Sleep):(t1, Wake):xs) =
      diffInMinutes t0 t1 + getTimeSleeping xs
    getTimeSleeping (_:xs) = getTimeSleeping xs

diffInMinutes :: [Int] -> [Int] -> Int
diffInMinutes [h, m] [h', m'] = (h' - h) * 60 + (m' - m)

justDigits :: String -> String
justDigits =
  map
    (\c ->
       if isDigit c
         then c
         else ' ')

head_ (x:_) = Just x
head_ _     = Nothing

sortKeysByValue :: Ord a => Map a Int -> (a, Int)
sortKeysByValue m = (fromJust f, fromJust $ M.lookup (fromJust f) m)
  where
    f = head_ $ go [] Nothing (M.toList m)
    go ks _ [] = ks
    go ks Nothing ((k, v):rest) = go (k : ks) (Just v) rest
    go ks (Just u) ((k, v):rest)
      | v < u = go ks (Just u) rest
      | v > u = go [k] (Just v) rest
      | otherwise = go (k : ks) (Just v) rest

getGuardIds :: [Shift] -> [GuardId]
getGuardIds []                    = []
getGuardIds (((_, Guard n):_):xs) = n : getGuardIds xs

solve' :: [Shift] -> GuardId -> [(GuardId, Minute, Int)]
solve' t g =
  map (\(m, n) -> (g, m, n)) $
  M.toList $ fromJust $ M.lookup g $ toMapGuardFreqs t

solve :: [Shift] -> [(GuardId, Minute, Int)]
solve s = sortBy (sortingFn) $ concatMap (solve' s) $ getGuardIds s
  where
    sortingFn (_, _, m) (_, _, n) = compare m n

main :: IO ()
main = do
  text <- sort . lines <$> readFile "input.txt"
  putStrLn $ show $ last $ solve $ groupEntries $ map parseLine text
