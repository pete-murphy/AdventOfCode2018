module Day13.Part1 where

import           Data.Array
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
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
  { dir   :: CartDirection
  , turns :: CartTurn
  } deriving (Eq)

getNextTurn :: CartTurn -> CartTurn
getNextTurn L = S
getNextTurn S = R
getNextTurn R = L

-- For sample 1 only
_parse1 :: String -> [String]
_parse1 =
  map (intersperse '\n') . filter (not . all isSpace) . transpose . lines

type TrackArray = Array (Int, Int) (Maybe Track)

type CartArray = Array (Int, Int) (Maybe Cart)

type Collision = (Int, Int)

findDuplicateCoord :: [((Int, Int), a)] -> Maybe (Int, Int)
findDuplicateCoord xs = findDuplicateCoord' xs'
  where
    xs' = sort $ map fst xs
    findDuplicateCoord' [] = Nothing
    findDuplicateCoord' [x] = Nothing
    findDuplicateCoord' (x:y:xs'') =
      if x == y
        then Just x
        else findDuplicateCoord' (y : xs'')

tick :: TrackArray -> CartArray -> Collision
tick ta ca = tick' [] (assocs ca)
  where
    tick' ::
         [((Int, Int), Maybe Cart)] -> [((Int, Int), Maybe Cart)] -> Collision
    tick' acc [] =
      case (findDuplicateCoord acc) of
        Nothing -> tick ta (ca // acc)
        Just x  -> x
    tick' acc (((x, y), Just (Cart Up t)):xs) =
      let nextCell = (x, y - 1)
          thisCell = (x, y)
       in case (ta ! nextCell, ca ! nextCell) of
            (_, Just _) -> nextCell
            (Just I, _) ->
              tick' (acc ++ [(thisCell, Nothing), (nextCell, nextTurn)]) xs
              where nextTurn =
                      case getNextTurn t of
                        S -> Just $ Cart Up S
                        L -> Just $ Cart Lt L
                        R -> Just $ Cart Rt R
            (Just CB, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Lt t))])
                xs
            (Just CF, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Rt t))])
                xs
            (Just V, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Up t))])
                xs
            _ ->
              error $
              "Something went wrong with up-facing cart" ++ show nextCell
    tick' acc (((x, y), Just (Cart Dn t)):xs) =
      let nextCell = (x, y + 1)
          thisCell = (x, y)
       in case (ta ! nextCell, ca ! nextCell) of
            (_, Just _) -> nextCell
            (Just I, _) ->
              tick' (acc ++ [(thisCell, Nothing), (nextCell, nextTurn)]) xs
              where nextTurn =
                      case getNextTurn t of
                        S -> Just $ Cart Dn S
                        L -> Just $ Cart Rt L
                        R -> Just $ Cart Lt R
            (Just CB, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Rt t))])
                xs
            (Just CF, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Lt t))])
                xs
            (Just V, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Dn t))])
                xs
            (x, _) ->
              error $
              "Something went wrong with down-facing cart" ++
              show nextCell ++
              "\n" ++
              show thisCell ++ "\n" ++ show x ++ "\n" ++ show (ta ! nextCell)
    tick' acc (((x, y), Just (Cart Rt t)):xs) =
      let nextCell = (x + 1, y)
          thisCell = (x, y)
       in case (ta ! nextCell, ca ! nextCell) of
            (_, Just _) -> nextCell
            (Just I, _) ->
              tick' (acc ++ [(thisCell, Nothing), (nextCell, nextTurn)]) xs
              where nextTurn =
                      case getNextTurn t of
                        S -> Just $ Cart Rt S
                        L -> Just $ Cart Up L
                        R -> Just $ Cart Dn R
            (Just CB, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Dn t))])
                xs
            (Just CF, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Up t))])
                xs
            (Just H, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Rt t))])
                xs
            _ ->
              error $
              "Something went wrong with right-facing cart" ++ show nextCell
    tick' acc (((x, y), Just (Cart Lt t)):xs) =
      let nextCell = (x - 1, y)
          thisCell = (x, y)
       in case (ta ! nextCell, ca ! nextCell) of
            (_, Just _) -> nextCell
            (Just I, _) ->
              tick' (acc ++ [(thisCell, Nothing), (nextCell, nextTurn)]) xs
              where nextTurn =
                      case getNextTurn t of
                        S -> Just $ Cart Lt S
                        L -> Just $ Cart Dn L
                        R -> Just $ Cart Up R
            (Just CB, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Up t))])
                xs
            (Just CF, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Dn t))])
                xs
            (Just H, _) ->
              tick'
                (acc ++ [(thisCell, Nothing), (nextCell, Just (Cart Lt t))])
                xs
            _ ->
              error $
              "Something went wrong with left-facing cart" ++ show nextCell
    tick' acc ((_, Nothing):xs) = tick' acc xs

-- This is gnarly but it works
showCartsAndTrack :: CartArray -> TrackArray -> String
showCartsAndTrack ca ta =
  unlines $
  chunksOf (n + 1) $
  concatMap snd $
  assocs
    (array (bounds ta) (fmap showMaybe <$> (assocs ta)) //
     (fmap showMaybe <$> (filter (not . (== Nothing) . snd) (assocs ca))))
  where
    showMaybe (Just x) = show x
    showMaybe Nothing  = " "
    n = maximum . map (fst . fst) . assocs $ ta

showTrack :: TrackArray -> String
showTrack ta =
  unlines . chunksOf (n + 1) . concatMap (maybe " " show . snd) . assocs $ ta
  where
    n = maximum . map (fst . fst) . assocs $ ta

showCarts :: CartArray -> String
showCarts ca =
  unlines . chunksOf (n + 1) . concatMap (maybe " " show . snd) . assocs $ ca
  where
    n = maximum . map (fst . fst) . assocs $ ca

parseTrack :: String -> TrackArray
parseTrack s =
  array ((0, 0), (x, y)) .
  map (\((y, x), t) -> ((x, y), t)) .
  assocs . listArray ((0, 0), (y, x)) . concatMap parseLine . lines $
  s
    -- Need to subtract 1 (array is 0-indexed)
  where
    x = subtract 1 $ length $ head $ lines s
    y = subtract 1 $ length $ lines s
    parseLine :: String -> [Maybe Track]
    parseLine []        = []
    parseLine ('-':xs)  = Just H : parseLine xs
    parseLine ('|':xs)  = Just V : parseLine xs
    parseLine ('\\':xs) = Just CB : parseLine xs
    parseLine ('/':xs)  = Just CF : parseLine xs
    parseLine ('+':xs)  = Just I : parseLine xs
    parseLine ('v':xs)  = Just V : parseLine xs
    parseLine ('^':xs)  = Just V : parseLine xs
    parseLine ('<':xs)  = Just H : parseLine xs
    parseLine ('>':xs)  = Just H : parseLine xs
    parseLine (_:xs)    = Nothing : parseLine xs

parseCarts :: String -> CartArray
parseCarts s =
  array ((0, 0), (x, y)) .
  map (\((y, x), c) -> ((x, y), c)) .
  assocs . listArray ((0, 0), (y, x)) . concatMap parseLine . lines $
  s
  where
    x = subtract 1 $ length $ head $ lines s
    y = subtract 1 $ length $ lines s
    parseLine :: String -> [Maybe Cart]
    parseLine []       = []
    parseLine ('v':xs) = Just (Cart Dn R) : parseLine xs
    parseLine ('^':xs) = Just (Cart Up R) : parseLine xs
    parseLine ('<':xs) = Just (Cart Lt R) : parseLine xs
    parseLine ('>':xs) = Just (Cart Rt R) : parseLine xs
    parseLine (_:xs)   = Nothing : parseLine xs

solve :: String -> Collision
solve t = tick ta ca
  where
    ta = parseTrack t
    ca = parseCarts t

main :: IO ()
main = do
  text <- readFile "src/Day13/input.txt"
  putStrLn $ show $ solve text
