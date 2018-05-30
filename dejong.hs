module DeJong (dejong) where

type Individual = [Float]

dejong :: Individual -> Float
dejong x = sum $ map (^2) x
