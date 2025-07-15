#' Summary for equivalence test result
#'
#' @param object An object of class "mixequivResults"
#' @param ... further arguments (ignored)
#'
#' @return A printed summary
#' @export
#' @method summary mixequivResults
summary.mixequivResults <- function(object, ...) {
  cat("Equivalence Test Summary\n")
  cat("-------------------------\n")
  cat(sprintf("  Estimate: %.3f\n", object$estimate))
  
  # Only show degrees of freedom if it exists (TOST/CI methods)
  if (!is.null(object$df)) {
    cat(sprintf("  Degrees of Freedom: %.3f\n", object$df))
  }
  
  cat(sprintf("  Standard Error: %.3f\n", object$se))
  
  # Handle TOST results (t-tests)
  if (!is.null(object$t1) && !is.null(object$t2)) {
    cat("\nTOST results:\n")
    cat(sprintf("  t1: %.3f, p1: %.3f\n", object$t1, object$p1))
    cat(sprintf("  t2: %.3f, p2: %.3f\n", object$t2, object$p2))
  }
  
  # Handle TOZT results (z-tests)
  if (!is.null(object$z1) && !is.null(object$z2)) {
    cat("\nTOZT results:\n")
    cat(sprintf("  z1: %.3f, p1: %.3f\n", object$z1, object$p1))
    cat(sprintf("  z2: %.3f, p2: %.3f\n", object$z2, object$p2))
  }
  
  # Handle CI results
  if (!is.null(object$ci)) {
    cat("\nConfidence Interval:\n")
    cat(sprintf("  [%.3f, %.3f]\n", object$ci["lower"], object$ci["upper"]))
  }
  
  # Show MMD (works for all methods)
  if (!is.null(object$mmd)) {
    if (length(object$mmd) == 2) {
      cat(sprintf("  MMD: [%.3f, %.3f]\n", object$mmd["lower"], object$mmd["upper"]))
    } else {
      cat(sprintf("  MMD: %.3f\n", object$mmd))
    }
  }
  
  cat("\nAlpha level:", object$alpha, "\n")
  cat("Result:", ifelse(object$equivalent, "Equivalent", "Not Equivalent"), "\n")
  invisible(object)
}
