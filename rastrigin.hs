module Rastrigin (rastrigin) where

type Individual = [Float]

fn :: Float -> Float
fn x = x ^ 2 - (10 * (cos $ 2 * pi * x))

rastriginLoop :: Individual -> Float
rastriginLoop (x:xs)
  | length xs == 0 = fn x
  | otherwise = (fn x) + (rastriginLoop xs)

rastrigin :: Individual -> Float
rastrigin x = 10 * (fromIntegral $ length x) + rastriginLoop x