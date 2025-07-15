test_that('TOZT() works', {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")

  # Load the necessary packages
  library(mixequiv)
  library(lme4)
  library(glmmTMB)
  library(tidyverse)

  # Load some data
  data(iris)

  df <- iris %>%
    select(Sepal.Width, Petal.Width, Species) %>%
    mutate(
      Species = as.factor(Species)
    )

  # Generalized linear model
  model <- glmer(Sepal.Width ~ Petal.Width + (1 | Species),
                 data = df, family = inverse.gaussian(link = "identity"),
                )

  # Define minimal meaningful difference (MMD)
  mmd <- 0.3

  # Test equivalence
  results <- TOZT(model, term = "Petal.Width", mmd = mmd, alpha = 0.05)

  # Check if the result is a list and contains expected elements
  expect_true(is.list(results))
  expect_true(all(c("estimate", "se", "z1", "p1", "z2", "p2", "alpha", "mmd", "equivalent") %in% names(results)))

  # Check if the equivalence is correctly evaluated
  expect_false(results$equivalent)


  # Generalized linear model
  model <- glmmTMB(Sepal.Width ~ Petal.Width + (1 | Species),
                 data = df, family = lognormal(link = "log"),
  )

  # Define minimal meaningful difference (MMD)
  mmd <- 0.3

  # Test equivalence
  results <- TOZT(model, term = "Petal.Width", mmd = mmd, alpha = 0.05)

  # Check if the result is a list and contains expected elements
  expect_true(is.list(results))
  expect_true(all(c("estimate", "se", "z1", "p1", "z2", "p2", "alpha", "mmd", "equivalent") %in% names(results)))

  # Check if the equivalence is correctly evaluated
  expect_false(results$equivalent)
})
