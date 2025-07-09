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
  if (lower >= mmd['lower'] && upper <= mmd['upper']) {
    return(TRUE)  # MMD is within the CI
  } else {
    return(FALSE) # MMD is outside the CI
  }
}
