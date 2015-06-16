module Cis194Bench (benchmarks) where

import Cis194

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "main" (nfIO main)
    ]
