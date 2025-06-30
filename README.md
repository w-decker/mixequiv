# mixequiv
Equivalence testing of linear mixed model fixed effects parameters

# Installation
```r
devtools::install_github("w-decker/mixequiv")
```

# Usage

## Some libraries we will use
```r
library(mixequiv)
library(lme4, quietly = T)
library(lmerTest, quietly = T) # MUST LOAD `lmerTest` to estimate DoF!
library(tidyverse, quietly = T)
```

## Load some data
```r
data(iris) 

df <- iris %>%
  select(Sepal.Width, Petal.Width, Species) %>%
  mutate(
    Species = as.factor(Species)
  )
```

## Define linear model
```r
model <- lmer(Sepal.Width ~ Petal.Width + (1 | Species), 
              data = df, REML = F, control = lmerControl(optimizer = "bobyqa"))
```

## Define minimal meaningful difference (MMD)
```r
mmd <- 0.3
```

## Test equivalence
```r
results <- test_equiv(model, term = "Petal.Width", method = "TOST", mmd = mmd)
```

## See results
```r
summary(results)
```
```yaml
Equivalence Test Summary
-------------------------
  Estimate: 0.741
  Degrees of Freedom: 147.399
  Standard Error: 0.119

TOST results:
  t1: 8.779, p1: 0.000
  t2: 3.717, p2: 1.000

Alpha level: 0.05 
Result: Not Equivalent 
```