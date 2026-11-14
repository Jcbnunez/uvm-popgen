# Gene Flow and Population Differentiation ($F_{ST}$)

---

# Learning Goals

By the end of this lecture you should be able to:

1. Define gene flow and migration.
2. Derive the basic migration equation for allele-frequency change.
3. Explain how migration influences allele frequencies across populations.
4. Describe population structure and why populations diverge spatially.
5. Define Wright's fixation index, $F_{ST}$.
6. Derive the heterozygosity-based formula for $F_{ST}$.
7. Interpret the biological meaning of $F_{ST}$ values.
8. Calculate $F_{ST}$ from allele-frequency data.
9. Explain why migration tends to reduce $F_{ST}$.
10. Connect gene flow to the maintenance of genetic variation across space.

---

# 1. The Limits of the Hardy-Weinberg Model

Hardy-Weinberg equilibrium assumes:

- Random mating
- No mutation
- No migration
- No selection

For much of this course, these assumptions have provided a useful null model.

However, natural populations rarely exist as one large, randomly mating unit.

Instead, populations are often distributed across space and connected by movement of individuals.

Examples include:

- Islands connected by dispersal
- Forest patches connected by seed movement
- Streams connected by fish migration
- Human populations connected by travel and migration

This raises a new question:

> What happens when alleles move between populations?

The evolutionary force that moves alleles through space is called gene flow.

---

# 2. What is Gene Flow?

Gene flow is the movement of alleles among populations through migration and reproduction.

Examples:

- Pollen moving between plant populations
- Individuals dispersing between habitat patches
- Marine larvae settling far from their birthplace
- Human migration between geographic regions

Gene flow is unique because:

> Unlike mutation, selection, or recombination, gene flow directly transfers existing genetic variation from one population to another.

Migration therefore acts as a homogenizing evolutionary force.

It tends to make populations more genetically similar.

---

# 3. A Simple Island-Continent Model

Consider two populations:

## Continent

Allele frequency:

$$
p_C
$$

---

## Island

Allele frequency:

$$
p_I
$$

---

Suppose migration occurs from the continent into the island.

Let

$$
m
$$

denote the migration rate.

Interpretation:

$$
m=P(\text{an island individual is a migrant})
$$

and

$$
1-m=P(\text{an island individual is not a migrant}).
$$

Our goal is to predict allele frequency on the island in the next generation.

---

# 4. Deriving the Migration Recursion

The future island population consists of two components.

## Residents

A fraction

$$
1-m
$$

comes from island residents.

These individuals contribute

$$
p_I(1-m).
$$

---

## Migrants

A fraction

$$
m
$$

comes from the continent.

These individuals contribute

$$
p_Cm.
$$

---

## Total

Adding both contributions:

$$
p_{I,t+1}=p_I(1-m)+p_Cm.
$$

This is the fundamental migration equation.

---

# 5. Deriving the Rate of Evolutionary Change

Define

$$
\Delta p_I=p_{I,t+1}-p_I.
$$

Substitute the recursion:

$$
\Delta p_I=p_I(1-m)+p_Cm-p_I.
$$

Expand:

$$
\Delta p_I=p_I-p_Im+p_Cm-p_I.
$$

Cancel terms:

$$
\Delta p_I=-p_Im+p_Cm.
$$

Factor out m:

$$
\Delta p_I=m(p_C-p_I).
$$

---

# 6. Interpreting the Migration Equation

The migration rate equation is

$$
\Delta p_I=m(p_C-p_I).
$$

Notice that two quantities determine evolutionary change.

---

## Factor 1: Migration Rate

$$
m
$$

If migration is rare, change is slow.

If migration is common, change is rapid.

---

## Factor 2: Population Differences

$$
(p_C-p_I)
$$

If populations are already similar,

$$
p_C \approx p_I
$$

then migration produces little change.

If populations are very different,

migration generates substantial allele-frequency change.

---

## Limiting Cases

### No migration

$$
m=0
$$

Therefore

$$
\Delta p_I=0.
$$

No change occurs.

---

### Identical populations

$$
p_C=p_I
$$

Therefore

$$
\Delta p_I=0.
$$

Migration occurs, but no allele-frequency differences exist to transfer.

---

### Maximum difference

Suppose

$$
p_C=1
$$

and

$$
p_I=0.
$$

Then

$$
\Delta p_I=m.
$$

The island frequency changes at the maximum possible rate.

---

# 7. Gene Flow as a Homogenizing Force

A useful way to think about migration is that it reduces differences among populations.

Suppose:

Population A:

$$
p_A=0.8
$$

Population B:

$$
p_B=0.2.
$$

Migration transfers alleles between them.

After many generations:

$$
p_A \rightarrow p_B.
$$

Eventually the populations become genetically similar.

Thus:

> Gene flow continually erodes genetic differences among populations.

This property makes migration the primary force opposing population differentiation.

---

# 8. Population Structure

So far we have largely assumed that a species behaves as a single population.

In reality many species are subdivided.

Common patterns include:

## Island Model

Many populations exchange migrants equally.

## Mainland-Island Model

A large source population supplies migrants to smaller populations.

## Stepping-Stone Model

Migration primarily occurs between neighboring populations.

## Metapopulation Model

Populations experience extinction, colonization, and migration.

The key consequence of population structure is that allele frequencies may differ among locations.

---

# 9. Measuring Population Differentiation

Suppose two populations have different allele frequencies.

How can we quantify that difference?

Sewall Wright developed a family of statistics called the F-statistics.

The statistic most relevant for studying migration is:

$$
F_{ST}.
$$

---

# 10. What is FST?

FST measures genetic differentiation among populations.

Conceptually:

$$
F_{ST}=
\text{fraction of genetic variation that exists among populations}.
$$

Range:

$$
0 \le F_{ST}\le 1.
$$

Interpretation:

| FST | Interpretation |
|------|------|
| 0 | No differentiation |
| 0.05 | Weak differentiation |
| 0.15 | Moderate differentiation |
| 0.25+ | Strong differentiation |
| 1 | Complete differentiation |

---

# 11. Heterozygosity and Population Structure

Recall that expected heterozygosity is

$$
H=2pq.
$$

Heterozygosity measures genetic variation.

To understand FST we compare two different heterozygosities.

---

## $H_S$

The average heterozygosity within subpopulations.

$$
H_S
$$

Think:

> How much variation exists inside populations?

---

## $H_T$

The heterozygosity obtained if all populations were pooled together.

$$
H_T
$$

Think:

> How much variation exists across the entire species?

---

# 12. Defining $F_{ST}$

Using heterozygosity:

$$
F_{ST}=
\frac{H_T-H_S}{H_T}.
$$

Interpretation:

Numerator:

$$
H_T-H_S
$$

measures variation lost when populations are considered separately.

Denominator:

$$
H_T
$$

standardizes the quantity.

Thus:

$$
F_{ST}=
\frac{\text{between-population variation}}
{\text{total variation}}.
$$

---

# 13. Biological Meaning of $F_{ST}$

Imagine all populations have identical allele frequencies.

Then

$$
H_S=H_T.
$$

Substituting:

$$
F_{ST}=0.
$$

In this case:

- No population structure exists.
- Migration has effectively homogenized populations.

---

Now imagine populations contain completely different alleles.

Then:

$$
H_S=0
$$

while

$$
H_T>0.
$$

Substituting:

$$
F_{ST}=1.
$$

All variation exists among populations.

This corresponds to complete differentiation.

---

# 14. Example: Complete Differentiation

Population 1:

$$
p_1=1
$$

Population 2:

$$
p_2=0.
$$

Within each population:

$$
H_1=2(1)(0)=0
$$

$$
H_2=2(0)(1)=0.
$$

Average heterozygosity:

$$
H_S=0.
$$

---

Calculate pooled frequency:

$$
\bar p=0.5.
$$

Therefore:

$$
\bar q=0.5.
$$

Pooled heterozygosity:

$$
H_T=2(0.5)(0.5)=0.5.
$$

Substituting:

$$
F_{ST}=
\frac{0.5-0}{0.5}=
1.
$$

All variation exists between populations.

---

# 15. Example: No Differentiation

Suppose

$$
p_1=0.5
$$

and

$$
p_2=0.5.
$$

Then:

$$
H_1=0.5
$$

and

$$
H_2=0.5.
$$

Average:

$$
H_S=0.5.
$$

The pooled population also has:

$$
H_T=0.5.
$$

Therefore:

$$
F_{ST}=
\frac{0.5-0.5}{0.5}=
0.
$$

There is no differentiation.

---

# 16. Why Migration Reduces FST

Migration tends to make allele frequencies more similar.

Suppose two populations begin with

$$
p_1=0.9
$$

and

$$
p_2=0.1.
$$

Migration gradually reduces this difference.

As allele frequencies converge:

$$
H_S
\rightarrow
H_T.
$$

Consequently:

$$
F_{ST}
\rightarrow
0.
$$

Thus:

> Gene flow reduces population differentiation.

This is one of the most important conceptual results in population genetics.

---

# 17. Connecting Gene Flow and FST

Migration and FST describe opposite aspects of the same biological process.

High migration:

- Populations exchange many alleles.
- Allele frequencies become similar.
- FST becomes small.

Low migration:

- Populations exchange few alleles.
- Allele frequencies diverge.
- FST becomes large.

Therefore:

$$
\text{Gene Flow}
\uparrow
\quad \Rightarrow \quad
F_{ST}
\downarrow
$$

and

$$
\text{Gene Flow}
\downarrow
\quad \Rightarrow \quad
F_{ST}
\uparrow.
$$

At this point in the course we should view FST primarily as:

> A measure of how effectively migration connects populations.

---

# 18. Interpreting Geographic Patterns

When populations are connected by migration, genetic differences tend to be small.

When migration is restricted by:

- distance,
- mountains,
- oceans,
- habitat fragmentation,

genetic differentiation tends to increase.

This is why genetic patterns often mirror geography. Population subdivision and restricted migration generate spatial genetic structure. The motivating examples in the source slides illustrate how genetic variation can be distributed geographically and how migration can maintain similarity among populations. 

---

# 19. Important Distinction

Notice that FST does not tell us:

- which population contributed migrants,
- whether a particular allele is beneficial,
- why populations differ.

Instead, FST tells us:

> How genetic variation is partitioned within versus among populations.

It is therefore a descriptive statistic of population structure.

---

# Key Take-Home Messages

1. Gene flow is the movement of alleles among populations. 

2. For an island-continent model,

$$
p_{I,t+1}=p_I(1-m)+p_Cm.
$$

3. The rate equation is

$$
\Delta p_I=m(p_C-p_I).
$$

4. Migration causes populations to become genetically similar through time.

5. Population structure occurs when allele frequencies differ among locations. 

6. FST measures genetic differentiation among populations. 

7. The heterozygosity definition is

$$
F_{ST}=\frac{H_T-H_S}{H_T}.
$$

8. FST = 0 indicates no differentiation.

9. FST = 1 indicates complete differentiation.

10. Migration and FST are inversely related:
    - high migration → low FST
    - low migration → high FST

11. FST quantifies how genetic variation is partitioned within and among populations.

12. We deliberately stop here before introducing genetic drift and effective population size. Later lectures will show how migration and drift jointly determine long-term patterns of population differentiation.