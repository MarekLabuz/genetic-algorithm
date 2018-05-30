import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Console.Haskeline
import System.Random (randomRIO)
import Control.Monad.Random
import Data.List
import Control.Monad.Trans.Reader

type Individual = [Float]
type Population = [Individual]
type Config = (Int, Int, Float, Float, Individual -> Float)

randomFloat :: Float -> Float -> IO Float
randomFloat min max = randomRIO (min, max)

randomIndividual :: Int -> Float -> Float -> IO Individual
randomIndividual dimension min max
  | dimension == 0 = return []
  | otherwise = do
    g <- randomFloat min max
    n <- randomIndividual (dimension - 1) min max
    return $ g:n

initPopulation :: Config -> IO Population
initPopulation (n, dimension, min, max, fn)
  | n == 0 = return []
  | otherwise = do
    i <- randomIndividual dimension min max
    n <- initPopulation ((n - 1), dimension, min, max, fn)
    return $ i:n

-- Rastrigin Function

fn :: Float -> Float
fn x = x ^ 2 - (10 * (cos $ 2 * pi * x))

rastriginLoop :: Individual -> Float
rastriginLoop (x:xs)
  | length xs == 0 = fn x
  | otherwise = (fn x) + (rastriginLoop xs)

rastrigin :: Individual -> Float
rastrigin x = 10 * (fromIntegral $ length x) + rastriginLoop x

--

-- DeJong Function

dejong :: Individual -> Float
dejong x = sum $ map (^2) x

--

computeFitness :: Config -> Population -> [Float]
computeFitness (_, _, _, _, fn) population = map fn population

sortIndividials :: (Individual, Float) -> (Individual, Float) -> Ordering
sortIndividials a b
  | (snd a) > (snd b) = GT
  | (snd a) < (snd b) = LT
  | otherwise = EQ

selectIndividuals :: RandomGen g => g -> [(Individual, Rational)] -> [Individual]
selectIndividuals gen weights = evalRand (sequence . repeat . fromList $ weights) gen

selection :: Population -> [Float] -> Population
selection population fitness = take len $ selectIndividuals (mkStdGen 1) $ zip sortedPopulation range
  where
    range = reverse $ map fromIntegral [0..length population]
    sortedPopulation = fst $ unzip $ sortBy sortIndividials $ zip population fitness
    len = length population

groupIntoPairs :: Population -> [(Individual, Individual)]
groupIntoPairs [] = []
groupIntoPairs l = (i1, i2):(groupIntoPairs $ drop 2 l) where
  [i1, i2] = take 2 l

mergePairs :: [(Individual, Individual)] -> Population
mergePairs [] = []
mergePairs ((i1, i2):xs) = i1:i2:(mergePairs xs)

exchangeGenes :: (Individual, Individual) -> (Individual, Individual)
exchangeGenes (i1, i2) = do
  let len = floor $ (fromIntegral $ length i1) / 3
  let i11 = take len i1
  let i21 = take len i2
  (i21 ++ (drop len i1), i11 ++ (drop len i2))

crossover :: Population -> Population
crossover population = mergePairs $ map exchangeGenes $ groupIntoPairs population

mutateGene :: Float -> Float -> Float -> IO Float
mutateGene min max g = do
  r <- randomFloat 0 1
  if r > 0.9 then randomFloat min max
  else return g

mutateIndividual :: Individual -> IO Individual
mutateIndividual x = sequence $ map (mutateGene (-40) 40) x

mutation :: Population -> IO Population
mutation p = sequence $ map mutateIndividual p

geneticLoop :: Int -> Population -> [Float] -> ReaderT Config IO Float
geneticLoop generations population fitness = do
  config <- ask
  if generations == 0 then return $ minimum $ computeFitness config population
  else do
    population <- liftIO $ initPopulation config
    newPopulation <- liftIO $ mutation $ crossover $ selection population $ computeFitness config population
    geneticLoop (generations - 1) newPopulation (computeFitness config newPopulation)

genetic = do
  let config = (1000, 2, 40.0, -40.0, rastrigin)
  population <- initPopulation config
  runReaderT (geneticLoop 100 population $ computeFitness config population) config


