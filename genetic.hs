import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Console.Haskeline
import System.Random (randomRIO)
import Data.List

type Individual = [Float]
type Population = [Individual]

randomFloat :: Float -> Float -> IO Float
randomFloat min max = randomRIO (min, max)

randomIndividual :: Int -> Float -> Float -> IO Individual
randomIndividual dimension min max
  | dimension == 0 = return []
  | otherwise = do
    i <- randomFloat min max
    n <- randomIndividual (dimension - 1) min max
    return $ i:n

-- initPopulation :: Int -> IO Population
-- initPopulation n =

