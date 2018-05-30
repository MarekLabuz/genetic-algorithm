import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Console.Haskeline
import System.Random (randomRIO)
import Control.Monad.Random
import Data.List

type Individual = [Float]
type Population = [Individual]

randomFloat :: Float -> Float -> IO Float
randomFloat min max = randomRIO (min, max)

randomIndividual :: Int -> Float -> Float -> IO Individual
randomIndividual dimension min max
  | dimension == 0 = return []
  | otherwise = do
    g <- randomFloat min max
    n <- randomIndividual (dimension - 1) min max
    return $ g:n

initPopulation :: Int -> Int -> Float -> Float -> IO Population
initPopulation n dimension min max
  | n == 0 = return []
  | otherwise = do
    i <- randomIndividual dimension min max
    n <- initPopulation (n - 1) dimension min max
    return $ i:n

-- Rastrigin Function

fn :: Float -> Float
fn x = x ^ 2 - (10 * (cos $ 2 * pi * x))

rastriginLoop :: Individual -> Float
rastriginLoop (x:rest)
  | length rest == 0 = fn x
  | otherwise = (fn x) + (rastriginLoop rest)

rastrigin :: Individual -> Float
rastrigin x = 10 * (fromIntegral $ length x) + rastriginLoop x

--

computeFitness :: Population -> [Float]
computeFitness population = map rastrigin population

sortIndividials :: (Individual, Float) -> (Individual, Float) -> Ordering
sortIndividials a b
  | (snd a) > (snd b) = GT
  | (snd a) < (snd b) = LT
  | otherwise = EQ

selectIndividuals :: RandomGen g => g -> [(Individual, Rational)] -> [Individual]
selectIndividuals gen weights = evalRand (sequence . repeat . fromList $ weights) gen

selection :: Population -> [Float] -> Population
selection population fitness = do
  let bestIndividuals = selectIndividuals (mkStdGen 1) $ zip sortedPopulation range
  take len bestIndividuals
  where
    range = reverse $ map fromIntegral [0..length population]
    sortedPopulation = fst $ unzip $ sortBy sortIndividials $ zip population fitness
    len = length population

groupIntoPairs :: Population -> [(Individual, Individual)]
groupIntoPairs [] = []
groupIntoPairs l = (i1, i2):(groupIntoPairs $ drop 2 l) where
  [i1, i2] = take 2 l

exchangeGenes :: (Individual, Individual) -> (Individual, Individual)
exchangeGenes pair =

crossover :: Population -> Population
crossover population = do
  let pairs = groupIntoPairs population
  pairs

-- genetic :: Int
genetic = do
  population <- initPopulation 100 10 (-40) 40
  let fitness = computeFitness population
  let newPopulation = selection population fitness
  return newPopulation
  -- population


