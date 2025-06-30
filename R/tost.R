source("R/get_lmer_values.R")

TOST <- function(model, mmd, term, alpha = 0.05) {
  #' Two one-sided t-test
  #'
  #' Compute two one-sided t-test with a desired alpha and minimal meaningful
  #' difference.
  #'
  #' @param model lmer object
  #' @param mmd minimal meaningful difference
  #' @param term model term
  #' @param alpha desired alpha value
  #' @return list of results
  #'
  #' @export

  if (!inherits(model, "lmerModLmerTest")) {
    stop("The argument 'model' must be of class 'lmerModLmerTest'.")
  }

  vals <- get_lmer_values(model, term, alpha = alpha)
  b <- vals[['estimate']]
  df <- vals[['df']]
  se <- vals[['se']]

  # t-test 1
  t1 <- (b - (-mmd)) / se
  p1 <- 1 - pt(t1, df = df)

  # t-test 2
  t2 <- (b - mmd) / se
  p2 <- pt(t2, df = df)

  # Determine equivalence
  equivalent <- max(p1, p2) < alpha

  # Return all values as a list
  return(list(
    estimate = b,
    df = df,
    se = se,
    t1 = t1,
    p1 = p1,
    t2 = t2,
    p2 = p2,
    alpha = alpha,
    mmd = mmd,
    equivalent = equivalent
  ))
}
