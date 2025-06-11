source("R/ci.R")
source("R/tost.R")
source("R/utils.R")

test_equiv <- function(model, method, mmd, term, alpha = 0.05) {
  #' Test equivalence of mixed model parameter
  #'
  #' Execute an equivalence test (TOST or CI approach) on a mixed model parameter
  #' with a specified minimal meaningful difference and desired alpha.
  #'
  #' @param model lmer object
  #' @param method desired equivalence test approach ("CI" or "TOST")
  #' @param mmd minimal meaningful difference
  #' @param term model term
  #' @param alpha desired alpha
  #'
  #' @return list of results
  #'
  #' @examples
    #' test_equiv(model, method = "TOST", mmd = 5, term = "X1:X2", alpha = 0.001)
    #' test_equiv(model, method = "CI", mmd = 0.1, term = "X1:X2", alpha = 0.05)
  #'
  #' @export

  if (!inherits(model, "lmerModLmerTest")) {
    stop("The argument 'model' must be of class 'lmerModLmerTest'.")
  }
  if (!is.character(method)) {
    stop("The argument 'method' must be an uppercase character vector.")
  }
  if (!is.numeric(mmd)) {
    stop("The argument 'mmd' must be numeric.")
  }

  if (!is.uppercase(method)){
    stop("The argument 'method' must be an uppercase character vector")
  }

  # make sure mmd is positive (handle [-mmd, +mmd] automatically)
  mmd = sign(mmd) * mmd

  if (method == "TOST") {
    return(TOST(model = model, mmd = mmd, term = term, alpha = alpha))
  } else if (method == "CI") {
    return(CI(model = model, mmd = mmd, term = term, alpha = alpha))
  } else {
    stop("Invalid method. Use 'TOST' or 'CI'.")
  }
}
