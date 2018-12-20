module Day19.Operations where

import           Data.Bits
import           Data.Vector (Vector)
import qualified Data.Vector as V

type Registers = Vector Int

addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr ::
     Registers -> (Int, Int, Int) -> Registers
addr rs (a, b, c) = rs V.// [(c, rs V.! a + rs V.! b)]

addi rs (a, b, c) = rs V.// [(c, rs V.! a + b)]

mulr rs (a, b, c) = rs V.// [(c, rs V.! a * rs V.! b)]

muli rs (a, b, c) = rs V.// [(c, rs V.! a * b)]

banr rs (a, b, c) = rs V.// [(c, rs V.! a .&. rs V.! b)]

bani rs (a, b, c) = rs V.// [(c, rs V.! a .&. b)]

borr rs (a, b, c) = rs V.// [(c, rs V.! a .|. rs V.! b)]

bori rs (a, b, c) = rs V.// [(c, rs V.! a .|. b)]

setr rs (a, _, c) = rs V.// [(c, rs V.! a)]

seti rs (a, _, c) = rs V.// [(c, a)]

gtir rs (a, b, c) =
  rs V.//
  [ ( c
    , if (a > rs V.! b)
        then 1
        else 0)
  ]

gtri rs (a, b, c) =
  rs V.//
  [ ( c
    , if (rs V.! a > b)
        then 1
        else 0)
  ]

gtrr rs (a, b, c) =
  rs V.//
  [ ( c
    , if (rs V.! a > rs V.! b)
        then 1
        else 0)
  ]

eqir rs (a, b, c) =
  rs V.//
  [ ( c
    , if (a == rs V.! b)
        then 1
        else 0)
  ]

eqri rs (a, b, c) =
  rs V.//
  [ ( c
    , if (rs V.! a == b)
        then 1
        else 0)
  ]

eqrr rs (a, b, c) =
  rs V.//
  [ ( c
    , if (rs V.! a == rs V.! b)
        then 1
        else 0)
  ]
