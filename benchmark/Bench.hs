module Main (main) where

import qualified Cis194Bench
-- HASKELETON: import qualified New.ModuleBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain
    [ bgroup "Cis194" Cis194Bench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
