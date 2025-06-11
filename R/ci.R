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

  # Print formatted output
  cat("Confidence Interval (CI) Results:\n")
  cat(sprintf("  Estimate (b): %.3f\n", b))
  cat(sprintf("  Degrees of Freedom (df): %.3f\n", df))
  cat(sprintf("  Standard Error (SE): %.3f\n", se))
  cat(sprintf("  Critical t-value (t_crit): %.3f\n", t_crit))
  cat(sprintf("  Confidence Interval: [%.3f, %.3f]\n", lower, upper))
  cat(sprintf("  Minimal Meaningful Difference (MMD): %.3f\n", mmd))
  cat(sprintf("  Alpha: %.3f\n", alpha))
  cat(sprintf("  Result: %s\n", ifelse(equivalent, "Equivalent", "Not Equivalent")))

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
