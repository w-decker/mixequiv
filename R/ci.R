source("R/utils.R")

evalci <- function(ci, mmd) {
  #' Compare MMD against confidence interval
  #'
  #' @param ci Named numeric vector with "lower" and "upper" elements
  #' @param mmd Minimal meaningful difference
  #' @return Logical value indicating whether MMD is within the CI
  #'
  #' @export

  # Ensure ci has the required names
  if (!all(c("lower", "upper") %in% names(ci))) {
    stop("The 'ci' parameter must be a named vector with 'lower' and 'upper'.")
  }

  lower <- ci["lower"]
  upper <- ci["upper"]

  # Check if MMD is within the confidence interval
  if (lower >= -mmd && upper <= mmd) {
    return(TRUE)  # MMD is within the CI
  } else {
    return(FALSE) # MMD is outside the CI
  }
}

CI <- function(model, mmd, term, alpha = 0.05) {
  #' Confidence interval equivalence test
  #'
  #' Compute an equivalence test using a symmetric confidence interval approach
  #'
  #' @param model lmer object
  #' @param mmd minimal meaningful difference
  #' @param term model term
  #' @param alpha desired alpha
  #'
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
  t_crit <- vals[['t_crit']]

  # Calculate confidence interval
  lower <- b - t_crit * se
  upper <- b + t_crit * se
  ci <- c(lower = lower, upper = upper)

  # Evaluate equivalence
  equivalent <- evalci(ci, mmd)

  # Return all values as a list
  return(list(
    estimate = b,
    df = df,
    se = se,
    t_crit = t_crit,
    ci = ci,
    mmd = mmd,
    alpha = alpha,
    equivalent = equivalent
  ))
}
