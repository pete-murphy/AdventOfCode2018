{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Day4.Part1 where

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
      [m0 .. m1] ++ getMinutesSleeping xs
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

exampleLine1 :: String
exampleLine1 = "[1518-05-11 00:00] Guard #2411 begins shift"

exampleLine2 :: String
exampleLine2 = "[1518-08-26 00:21] wakes up"

example :: [String]
example =
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  , "[1518-11-02 00:40] falls asleep"
  , "[1518-11-02 00:50] wakes up"
  , "[1518-11-03 00:05] Guard #10 begins shift"
  , "[1518-11-03 00:24] falls asleep"
  , "[1518-11-03 00:29] wakes up"
  , "[1518-11-04 00:02] Guard #99 begins shift"
  , "[1518-11-04 00:36] falls asleep"
  , "[1518-11-04 00:46] wakes up"
  , "[1518-11-05 00:03] Guard #99 begins shift"
  , "[1518-11-05 00:45] falls asleep"
  , "[1518-11-05 00:55] wakes up"
  ]

diffInMinutes :: [Int] -> [Int] -> Int
diffInMinutes [h, m] [h', m'] = (h' - h) * 60 + (m' - m)

justDigits :: String -> String
justDigits =
  map
    (\c ->
       if isDigit c
         then c
         else ' ')

sortKeysByValue m = go [] Nothing (M.toList m)
  where
    go ks _ [] = ks
    go ks Nothing ((k, v):rest) = go (k : ks) (Just v) rest
    go ks (Just u) ((k, v):rest)
      | v < u = go ks (Just u) rest
      | v > u = go [k] (Just v) rest
      | otherwise = go (k : ks) (Just v) rest

solve' :: GuardId -> [String] -> Minute
solve' g t =
  head $
  sortKeysByValue $
  fromJust $ M.lookup g $ toMapGuardFreqs $ groupEntries $ map parseLine t

solve :: [String] -> GuardId
solve =
  head . sortKeysByValue . toMapGuardMinutes . groupEntries . map parseLine

main :: IO ()
main = do
  text <- sort . lines <$> readFile "input.txt"
  let guardId = solve text
      minute = solve' guardId text
  putStrLn $ show $ (guardId, minute)
