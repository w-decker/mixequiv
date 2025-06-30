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
  #' @importFrom stats pt qt
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
