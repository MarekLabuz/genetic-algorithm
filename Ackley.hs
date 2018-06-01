module Ackley (ackley) where

type Individual = [Float]

a = 20
b = 0.2
c = 2 * pi

fn :: Float -> (Float, Float) -> (Float, Float)
fn x (p, q) = (p + x ^ 2, q + cos(c * x))

ackleyLoop :: Individual -> (Float, Float)
ackleyLoop (x:xs)
  | length xs == 0 = fn x (0, 0)
  | otherwise = fn x $ ackleyLoop xs

ackley :: Individual -> Float
ackley x = do
  let d = fromIntegral $ length x
  let (sum1, sum2) = ackleyLoop x
  let term1 = -a * (exp $ -b * (sqrt $ sum1 / d))
  let term2 = -1 * (exp $ sum2 / d)
  term1 + term2 + a + (exp 1)
