
# Recitation 1

Complete the following activity

Objective:

Learn to use R to solve a Hardy-Weinberg equilibrium (HWE) problem, including calculating genotype frequencies, testing for equilibrium, and visualizing results (5 questions).

#### 1. Introduction to Hardy-Weinberg Principle

The Hardy-Weinberg principle states that in a large, random-mating population with no mutation, migration, or selection, the allele and genotype frequencies remain constant across generations.

Imagine a population where the genotype counts are:

-   AA =50
    
-   Aa=30
    
-   aa=20
    
We'll calculate allele frequencies, expected genotype frequencies under HWE, and test for equilibrium using R.

```r
##### Step 1: Input Data

# Genotype counts
AA <- 50
Aa <- 30
aa <- <<QUESTION 1: what should this number be?>>

# Total population size
N <- AA + Aa + aa
N
```

##### Step 2: Calculate Observed Allele Frequencies
```r
# Observed allele frequencies

p <- (2 * AA + Aa) / (2 * N) # Frequency of A

q <- <<QUESTION 2: what should this formula be?>> # Frequency of a
```
  

##### Step 3: Expected Genotype Frequencies Under HWE
```r
# Expected genotype frequencies

exp_AA <- p^2

exp_Aa <- 2 * p * q

exp_aa <- q^2
```
  

##### Step 4: Expected Genotype Counts
```r
# Convert frequencies to counts

exp_counts <- c(AA = exp_AA * N, Aa = exp_Aa * N, aa = exp_aa * N)

QUESTION 3: explain in words what is occurring on this line of code?
```
