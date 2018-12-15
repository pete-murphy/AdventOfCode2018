module Day13.Part1 where

import           Data.Array
import           Data.Char
import           Data.List

data CartDirection
  = Up
  | Dn
  | Lt
  | Rt
  deriving (Eq)

data CartTurn
  = L
  | S
  | R
  deriving (Eq)

data TurnDirection
  = LTurn
  | RTurn

data Track
  = H
  | V
  | CF
  | CB
  | I
  deriving (Eq)

instance Show Track where
  show H  = "-"
  show V  = "|"
  show CF = "\\"
  show CB = "/"
  show I  = "+"

data Cart = Cart
  { dir   :: CartDirection
  , turns :: CartTurn
  }

getNextTurn :: CartTurn -> CartTurn
getNextTurn L = S
getNextTurn S = R
getNextTurn R = L

-- | For sample 1 only
_parse1 :: String -> [String]
_parse1 =
  map (intersperse '\n') . filter (not . all isSpace) . transpose . lines

type TrackArray = Array (Int, Int) (Track)

type CartArray = Array (Int, Int) (Maybe Cart)

type Collision = (Int, Int)

tick :: TrackArray -> CartArray -> Either Collision CartArray
tick ta ca = tick' (assocs ca)
  where
    tick' :: [((Int, Int), (Maybe Cart))] -> Either Collision CartArray
    tick' (((x, y), Just (Cart Up t)):xs) =
      case (ta ! (x - 1, y), ca ! (x - 1, y)) of
        (_, Just _) -> Left (x - 1, y)
        (I, _) -> Right $ ca // [((x - 1, y), next)]
          where next =
                  case getNextTurn t of
                    S -> Just $ Cart Up S
                    L -> Just $ Cart Lt L
                    R -> Just $ Cart Rt R
        (CB, _) -> Right $ ca // [((x - 1, y), Just (Cart Rt t))]

parseTrack :: String -> TrackArray
parseTrack s = listArray ((0, 0), (x, y)) . concatMap parseLine . lines $ s
  where
    x = length $ head $ lines s
    y = length $ lines s
    parseLine :: String -> [Track]
    parseLine []        = []
    parseLine ('-':xs)  = H : parseLine xs
    parseLine ('|':xs)  = V : parseLine xs
    parseLine ('\\':xs) = CB : parseLine xs
    parseLine ('/':xs)  = CF : parseLine xs
    parseLine ('+':xs)  = I : parseLine xs
    parseLine ('v':xs)  = V : parseLine xs
    parseLine ('^':xs)  = V : parseLine xs
    parseLine ('<':xs)  = H : parseLine xs
    parseLine ('>':xs)  = H : parseLine xs
    parseLine (_:xs)    = parseLine xs

parseCarts :: String -> CartArray
parseCarts s = listArray ((0, 0), (x, y)) . concatMap parseLine . lines $ s
  where
    x = length $ head $ lines s
    y = length $ lines s
    parseLine :: String -> [Maybe Cart]
    parseLine []       = []
    parseLine ('v':xs) = Just (Cart Dn R) : parseLine xs
    parseLine ('^':xs) = Just (Cart Up R) : parseLine xs
    parseLine ('<':xs) = Just (Cart Lt R) : parseLine xs
    parseLine ('>':xs) = Just (Cart Rt R) : parseLine xs
    parseLine (_:xs)   = Nothing : parseLine xs
