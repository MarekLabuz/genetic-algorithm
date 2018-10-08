## Genetic algorithm

The algorithm consists of four steps:
 - calculating fitness of each individual in a population (can be done in parallel)
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

Example output for Schwefel function:
```
([418.93805,389.37775,420.706,421.37262,418.93805,409.66266,418.93805,421.37262,421.75006,418.93805,418.93805,421.37262,420.706,421.37262,418.93805,418.93805,421.37262,420.706,420.706,421.37262,420.706,421.37262,420.706,-277.5094,421.37262,421.37262,421.75006,418.93805,418.93805,421.37262,420.706,420.706,420.706,420.706,421.37262,420.706,420.706,409.66266,420.706,420.706,421.37262,421.37262,409.66266,420.706,420.706,420.706,418.93805,421.75006,420.706,418.93805],4.9492188) in 20.83412s
```
It is a tuple where
- first element is an array of arguments for which a value of Schwefel function was found to be minimal
- second element is a found minimum of the function

Additionally, there is an execution time.

Expected results:
<p>
<img src="https://www.sfu.ca/~ssurjano/schwef3.png">

<sub>Image source: https://www.sfu.ca/~ssurjano/schwef.html</sub>
</p>
