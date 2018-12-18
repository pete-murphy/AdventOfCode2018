{-# LANGUAGE LambdaCase #-}

module Day13.Part2 where

import           Data.Array
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Ord
import           Debug.Trace

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
  deriving (Eq, Ord, Show)

-- Not sure if this is a good idea
instance Enum CartTurn where
  succ L = S
  succ S = R
  succ R = L
  toEnum 0 = L
  toEnum 1 = S
  toEnum 2 = R
  fromEnum L = 0
  fromEnum S = 1
  fromEnum R = 2

data Track
  = H
  | V
  | CF
  | CB
  | I
  deriving (Eq)

instance Show Cart where
  show (Cart Up _) = "^"
  show (Cart Dn _) = "v"
  show (Cart Rt _) = ">"
  show (Cart Lt _) = "<"

instance Show Track where
  show H  = "-"
  show V  = "|"
  show CB = "\\"
  show CF = "/"
  show I  = "+"

data Cart = Cart
  { heading  :: CartDirection
  , lastTurn :: CartTurn
  } deriving (Eq)

newtype Coord = Coord
  { unCoord :: (Int, Int)
  } deriving (Eq, Show)

instance Ord Coord where
  compare (Coord (x, y)) (Coord (x', y')) = compare (y, x) (y', x')

type TrackMap = Map Coord Track

type CartMap = Map Coord Cart

findLastCart :: Map Coord Cart -> Maybe Coord
findLastCart cs
  | M.size cs > 1 = Nothing
  | otherwise = listToMaybe $ M.keys cs

cartMove :: Coord -> Cart -> Coord
cartMove (Coord (x, y)) (Cart h _) =
  case h of
    Up -> Coord (x, y - 1)
    Dn -> Coord (x, y + 1)
    Rt -> Coord (x + 1, y)
    Lt -> Coord (x - 1, y)

-- There must be a better way :<
cartTurn :: Cart -> Track -> Cart
cartTurn (Cart h t) track =
  case track of
    I ->
      case (h, t) of
        (Up, L) -> Cart Lt (succ t)
        (Up, S) -> Cart Up (succ t)
        (Up, R) -> Cart Rt (succ t)
        (Rt, L) -> Cart Up (succ t)
        (Rt, S) -> Cart Rt (succ t)
        (Rt, R) -> Cart Dn (succ t)
        (Dn, L) -> Cart Rt (succ t)
        (Dn, S) -> Cart Dn (succ t)
        (Dn, R) -> Cart Lt (succ t)
        (Lt, L) -> Cart Dn (succ t)
        (Lt, S) -> Cart Lt (succ t)
        (Lt, R) -> Cart Up (succ t)
    CB ->
      case h {- CB is \ -}
            of
        Up -> Cart Lt t
        Dn -> Cart Rt t
        Lt -> Cart Up t
        Rt -> Cart Dn t
    CF ->
      case h {- CF is / -}
            of
        Up -> Cart Rt t
        Dn -> Cart Lt t
        Lt -> Cart Dn t
        Rt -> Cart Up t
    _ -> Cart h t

tick :: TrackMap -> CartMap -> Coord
tick tmap cmap = traceShow (M.size cmap) tick' (M.assocs cmap) M.empty
  where
    tick' :: [(Coord, Cart)] -> Map Coord Cart -> Coord
    tick' [] acc =
      case findLastCart acc of
        Nothing -> tick tmap acc
        Just x  -> x
    tick' ((coord, cart):xs) acc =
      let nextCoord = cartMove coord cart
          cart' = cartTurn cart (tmap M.! nextCoord)
       in case acc M.!? nextCoord of
            Nothing -> tick' xs $ M.insert nextCoord cart' acc
            Just _  -> tick' xs $ M.delete nextCoord acc

withCoords :: [String] -> [(Coord, Char)]
withCoords =
  concat .
  zipWith (\y -> map (\(x, c) -> (Coord (x, y), c))) [0 ..] . map (zip [0 ..])

charToTrack :: Char -> Maybe Track
charToTrack =
  \case
    '-' -> Just H
    '|' -> Just V
    '\\' -> Just CB
    '/' -> Just CF
    '+' -> Just I
    'v' -> Just V
    '^' -> Just V
    '<' -> Just H
    '>' -> Just H
    _ -> Nothing

charToCart :: Char -> Maybe Cart
charToCart =
  \case
    'v' -> Just $ Cart Dn L
    '^' -> Just $ Cart Up L
    '>' -> Just $ Cart Rt L
    '<' -> Just $ Cart Lt L
    _ -> Nothing

parseTrack :: String -> TrackMap
parseTrack =
  M.fromList . catMaybes' . map (fmap charToTrack) . withCoords . lines

parseCarts :: String -> CartMap
parseCarts =
  M.fromList . catMaybes' . map (fmap charToCart) . withCoords . lines

catMaybes' []                = []
catMaybes' ((c, Just x):xs)  = (c, x) : catMaybes' xs
catMaybes' ((c, Nothing):xs) = catMaybes' xs

solve :: String -> (Int, Int)
solve t = unCoord $ tick ta ca
  where
    ta = parseTrack t
    ca = parseCarts t

main :: IO ()
main = do
  text <- readFile "src/Day13/input.txt"
  putStrLn $ show $ solve text
