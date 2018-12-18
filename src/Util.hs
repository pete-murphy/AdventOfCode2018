module Util where

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f

withIndices :: [[a]] -> [((Int, Int), a)]
withIndices =
  concat .
  zipWith (\y -> map (\(x, c) -> ((x, y), c))) [0 ..] . map (zip [0 ..])
