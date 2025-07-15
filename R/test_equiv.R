test_equiv <- function(model, method, mmd, mmd_lower, mmd_upper, term, alpha = 0.05) {
  #' Test equivalence of mixed model parameter
  #'
  #' Execute an equivalence test (TOST or CI approach) on a mixed model parameter
  #' with a specified minimal meaningful difference and desired alpha.
  #'
  #' @param model lmer object
  #' @param method desired equivalence test approach ("CI" or "TOST")
  #' @param mmd minimal meaningful difference
  #' @param mmd_lower minimal meaningful difference lower boundary
  #' @param mmd_upper minimal meaningful difference upper boundary
  #' @param term model term
  #' @param alpha desired alpha
  #'
  #' @return list of results
  #'
  #' @export

  if (!any(inherits(model, "lmerModLmerTest"), inherits(model, "glmerMod"), inherits(model, "glmmTMB"))) {
    stop("The argument 'model' must be of class 'lmerModLmerTest' or 'glmerMod' or 'glmmTMB'")
  }
  if (!is.character(method)) {
    stop("The argument 'method' must be an uppercase character vector")
  }
  if (!is.numeric(mmd)) {
    stop("The argument 'mmd' must be numeric.")
  }

  if (!is.uppercase(method)){
    stop("The argument 'method' must be an uppercase character vector")
  }

  if (!missing(mmd) && (missing(mmd_lower) || missing(mmd_upper))) {
    mmd <- c(lower = -abs(mmd), upper = abs(mmd))
  } else if (!missing(mmd_lower) && !missing(mmd_upper)) {
    if (mmd_lower > mmd_upper) {
      stop("'mmd_lower' must be less than 'mmd_upper'")
    }
    mmd <- c(lower = mmd_lower, upper = mmd_upper)
  } else {
    stop("You must provide either 'mmd' or both 'mmd_lower' and 'mmd_upper'")
  }

  if (!any(inherits(model, "glmerMod"), inherits(model, "glmmTMB"))){
     if (method == "TOST") {
      res <- TOST(model = model, mmd = mmd, term = term, alpha = alpha)
    } else if (method == "CI") {
      res <- CI(model = model, mmd = mmd, term = term, alpha = alpha)
    } else if (method == "TOZT") {
      res <- TOZT(model = model, mmd = mmd, term = term, alpha = alpha)
    } else {
      stop("Invalid method. Use 'TOST', 'CI', or 'TOZT'.")
    }
  } else {
    if (method != "TOZT") {
      stop("For 'glmerMod' or 'glmmTMB' models, only the 'TOZT' method is available.")
    }
    res <- TOZT(model = model, mmd = mmd, term = term, alpha = alpha)
  }

  class(res) <- "mixequivResults"
  return(res)
}
