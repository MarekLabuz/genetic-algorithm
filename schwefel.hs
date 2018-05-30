module Schwefel (schwefel) where

type Individual = [Float]

schwefel :: Individual -> Float
schwefel x = do
  let s =  sum $ map (\v -> (*v) . sin . sqrt . abs $ v) x
  418.9829 * (fromIntegral $ length x) - s
