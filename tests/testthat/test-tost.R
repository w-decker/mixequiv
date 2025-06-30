test_that('TOST() works', {
  skip_on_cran()
  
  # Load the necessary packages
  library(mixequiv)
  library(lme4)
  library(lmerTest)
  library(tidyverse)

  # Load some data
  data(iris) 

  df <- iris %>%
    select(Sepal.Width, Petal.Width, Species) %>%
    mutate(
      Species = as.factor(Species)
    )

  # Linear model
  model <- lmer(Sepal.Width ~ Petal.Width + (1 | Species),
                data = df, REML = F,
                control = lmerControl(optimizer = "bobyqa"))
  
  # Define minimal meaningful difference (MMD)
  mmd <- 0.3
  
  # Test equivalence
  results <- TOST(model, term = "Petal.Width", mmd = mmd)
  
  # Check if the result is a list and contains expected elements
  expect_true(is.list(results))
  expect_true(all(c("estimate", "df", "se", "t1", "p1", "t2", "p2", "alpha", "mmd", "equivalent") %in% names(results)))
  
  # Check if the equivalence is correctly evaluated
  expect_false(results$equivalent)
})