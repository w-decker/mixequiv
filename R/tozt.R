TOZT <- function(model, mmd, term, alpha) {
  #' Two one sided z-test (Wald test)
  #'
  #' Compute two one-sided z-test with a desired alpha and minimal meaningful
  #' difference.
  #' 
  #' @param model lmer object
  #' @param mmd minimal meaningful difference
  #' @param term model term
  #' @param alpha desired alpha value
  #' @importFrom stats pt qt pnorm
  #' @return list of results
  #' 
  #' @export
  
  
  if (!any(inherits(model, "lmerModLmerTest"), inherits(model, "glmerMod"), inherits(model, "glmmTMB"))) {
    stop("The argument 'model' must be of class 'lmerModLmerTest' or 'glmerMod' or 'glmmTMB'")
  }

  if (is.null(names(mmd)) && length(mmd) == 1) {
    mmd <- c(lower = -abs(mmd), upper = abs(mmd))
  }

  vals <- get_lmer_values(model, term, alpha = alpha)
  b <- vals[['estimate']]
  se <- vals[['se']]
  
  # z-test 1
  z1 <- (b - (mmd['lower'])) / se
  p1 <- 1 - pnorm(z1)

# z-test 2
z2 <- (b - mmd['upper']) / se
p2 <- pnorm(z2)

# Determine equivalence
equivalent <- max(p1, p2) < alpha

# Return all values as a list
return(list(
    estimate = b,
    se = se,
    z1 = z1,
    p1 = p1,
    z2 = z2,
    p2 = p2,
    alpha = alpha,
    mmd = mmd,
    equivalent = equivalent
))
}