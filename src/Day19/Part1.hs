{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Day19.Part1 where

import           Control.Arrow
import           Data.Bits
import           Data.List.Split
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           Day19.Operations (Registers, addi, addr, bani, banr, bori,
                                   borr, eqir, eqri, eqrr, gtir, gtri, gtrr,
                                   muli, mulr, seti, setr)
import           Debug.Trace

data Instruction = Instruction
  { _op    :: Operation
  , inputA :: Int
  , inputB :: Int
  , output :: Int
  }

data Operation
  = AddR
  | AddI
  | MulR
  | MulI
  | BAnR
  | BAnI
  | BOrR
  | BOrI
  | SetR
  | SetI
  | GTIR
  | GTRI
  | GTRR
  | EqIR
  | EqRI
  | EqRR
  deriving (Eq)

runOperation :: Operation -> Registers -> (Int, Int, Int) -> Registers
runOperation =
  \case
    AddR -> addr
    AddI -> addi
    MulR -> mulr
    MulI -> muli
    BAnR -> banr
    BAnI -> bani
    BOrR -> borr
    BOrI -> bori
    SetR -> setr
    SetI -> seti
    GTIR -> gtir
    GTRI -> gtri
    GTRR -> gtrr
    EqIR -> eqir
    EqRI -> eqri
    EqRR -> eqrr

toTuple :: [a] -> (a, a, a)
toTuple (a:b:c:_) = (a, b, c)

type IPointer = Int

parseInstructions :: [String] -> Map IPointer (Operation, (Int, Int, Int))
parseInstructions ls = M.fromList $ zip [0 ..] ls'
  where
    ls' :: [(Operation, (Int, Int, Int))]
    ls' = map (parse' . words) ls
    parse' :: [String] -> (Operation, (Int, Int, Int))
    parse' (op:rest) = (op', toTuple $ map read rest)
      where
        op' =
          case op of
            "addr" -> AddR
            "addi" -> AddI
            "mulr" -> MulR
            "muli" -> MulI
            "banr" -> BAnR
            "bani" -> BAnI
            "borr" -> BOrR
            "bori" -> BOrI
            "setr" -> SetR
            "seti" -> SetI
            "gtir" -> GTIR
            "gtri" -> GTRI
            "gtrr" -> GTRR
            "eqir" -> EqIR
            "eqri" -> EqRI
            "eqrr" -> EqRR

parseIp :: String -> IPointer
parseIp = read . last . words

type IPointerReg = Int

runProgram ::
     IPointerReg
  -> Map IPointer (Operation, (Int, Int, Int))
  -> Registers
  -> Registers
runProgram ipr mapInstructions rs =
  case mapInstructions M.!? (rs V.! ipr) of
    Nothing -> rs
    Just (op, (a, b, c)) ->
      runProgram
        ipr
        mapInstructions
        (incrementIPointer ipr $ runOperation op rs (a, b, c))

incrementIPointer :: IPointerReg -> Registers -> Registers
incrementIPointer ipr rs = rs V.// [(ipr, (rs V.! ipr) + 1)]

solve :: String -> Registers
solve text = runProgram ip mapInstructions $ V.fromList [0, 0, 0, 0, 0, 0]
  where
    (ip, mapInstructions) =
      (parseIp . head &&& parseInstructions . tail) $ lines text

main :: IO ()
main = do
  text <- readFile "src/Day19/input.txt"
  putStrLn $ show $ solve text
