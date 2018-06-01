import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Console.Haskeline
import System.Random (randomRIO)
import Control.Monad.Random
import Data.List
import Control.Monad.Trans.Reader
import Control.Parallel.Strategies
import Data.Time

import Rastrigin (rastrigin)
import DeJong (dejong)
import Schwefel (schwefel)

type Individual = [Float]
type Population = [Individual]
type Config = (Int, Int, Float, Float, Individual -> Float)
data TestFunctions = DeJong | Rastrigin | Schwefel

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

computeFitness :: Config -> Population -> [Float]
computeFitness (_, _, _, _, fn) population = map fn population `using` parList rpar

sortIndividials :: (Individual, Float) -> (Individual, Float) -> Ordering
sortIndividials a b
  | (snd a) > (snd b) = GT
  | (snd a) < (snd b) = LT
  | otherwise = EQ

selectIndividuals :: RandomGen g => g -> [(Individual, Rational)] -> [Individual]
selectIndividuals gen weights = evalRand (sequence . repeat . fromList $ weights) gen

selection :: Population -> [Float] -> IO Population
selection population fitness = do
  r <- randomRIO (1, 10000)
  return $ take len $ selectIndividuals (mkStdGen r) probability
  where
    zipped = zip population fitness
    m = (maximum fitness) + 1
    probability = map (\(p, f) -> (p, fromIntegral $ round $ m - f)) zipped
    len = length population

groupIntoPairs :: Population -> [(Individual, Individual)]
groupIntoPairs [] = []
groupIntoPairs l = (i1, i2):(groupIntoPairs $ drop 2 l) where [i1, i2] = take 2 l

mergePairs :: [(Individual, Individual)] -> Population
mergePairs [] = []
mergePairs ((i1, i2):xs) = i1:i2:(mergePairs xs)

exchangeGenes :: (Individual, Individual) -> IO (Individual, Individual)
exchangeGenes (i1, i2) = do
  len <- randomRIO (1, length i1)
  side <- randomFloat 0 1
  let (f1, f2) = if' (side > 0.5) (take, drop) (drop, take)
  let i11 = f1 len i1
  let i21 = f1 len i2
  return (i21 ++ (f2 len i1), i11 ++ (f2 len i2))

crossover :: Population -> IO Population
crossover population = do
  s <- sequence (map exchangeGenes (groupIntoPairs population) `using` parList rpar)
  return $ mergePairs s

mutateGene :: Float -> Float -> Float -> IO Float
mutateGene min max g = do
  r <- randomFloat 0 1
  if r > 0.99 then randomFloat min max
  else return g

mutateIndividual :: Config -> Individual -> IO Individual
mutateIndividual (_, _, min, max, _) x = sequence (map (mutateGene min max) x `using` parList rpar)

mutation :: Config -> Population -> IO Population
mutation config p = sequence (map (mutateIndividual config) p `using` parList rpar)

avg :: [Float] -> Float
avg a = (sum a) / (fromIntegral $ length a)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

geneticLoop :: ReaderT Config (StateT ((Individual, Float), Population, [Float], Int) IO) (Individual, Float)
geneticLoop = do
  config <- ask
  (lowest, population, fitness, generations) <- lift get
  selected <- liftIO $ selection population fitness
  crossedOver <- liftIO $ crossover selected
  newPopulation <- liftIO $ mutation config crossedOver
  let newFitness = computeFitness config newPopulation
  let newLowest = minimumBy sortIndividials $ zip population newFitness
  let l = if' (snd newLowest < snd lowest) newLowest lowest
  lift $ put (l, newPopulation, newFitness, generations - 1)
  liftIO $ putStrLn $ show $ snd l
  if generations == 0 then return l else geneticLoop

testFunctions :: TestFunctions -> Config
testFunctions name = case name of
  DeJong -> (100, 50, -40.0, 40.0, dejong)
  Rastrigin -> (100, 50, -40.0, 40.0, rastrigin)
  Schwefel -> (100, 50, -500.0, 500.0, schwefel)

genetic :: TestFunctions -> IO (Individual, Float)
genetic name = do
  let config = testFunctions name
  population <- initPopulation config
  let fitness = computeFitness config population
  let low = minimumBy sortIndividials $ zip population fitness
  r <- runStateT (runReaderT geneticLoop config) (low, population, fitness, 1000)
  return $ fst r

main = do
  start <- getCurrentTime
  r <- genetic Schwefel
  end <- getCurrentTime
  putStrLn $ (show r) ++ " in " ++ (show $ diffUTCTime end start)
