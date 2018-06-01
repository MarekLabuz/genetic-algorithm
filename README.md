## Genetic algorithm

Initially, I created game "Battleships", but then I changed my mind, because the game turned out to be not a good use case for parallel execution. Therefore, I have written genetic algorithm in Haskell.

The algorithm consists of four steps:
 - calculating fitness of each individual in population (can be done in parallel)
 - selection of best individuals based on their fitness
 - crossover (can be done in parallel)
 - mutation (can be done in parallel)

These steps are executed in a loop, each run generates a new generation of a population.

Parameters used for the algorithm:
 - Number of individuals (population): 120
 - Dimmension of a problem to be solved: 50
 - Number of generations: 1000
 - Probability of crossover: 70%
 - Probability of mutation: 1%

Three optimization problems have been implemented. The goal is to find minimum of these functions:

- Ackley function: https://www.sfu.ca/~ssurjano/ackley.html
- Schwefel function: https://www.sfu.ca/~ssurjano/schwef.html
- Rastrigin function: https://www.sfu.ca/~ssurjano/rastr.html
