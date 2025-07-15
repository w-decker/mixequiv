get_lmer_values <- function(model, term, alpha = 0.05) {
  #' Get values from lmer object
  #'
  #' Extract a specific model parameter's estimate, degrees of freedom and error
  #'
  #' @param model lmer object
  #' @param term model term
  #' @param alpha desired alpha value
  #' @return list of values
  #'
  #' @export

  if (!any(inherits(model, "lmerModLmerTest"), inherits(model, "glmerMod"), inherits(model, "glmmTMB"))) {
    stop("The argument 'model' must be of class 'lmerModLmerTest' or 'glmerMod' or 'glmmTMB'")
  }

  # Extract summary coefficients
  if (!inherits(model, "glmmTMB")) {
    coefficients <- summary(model)$coefficients
  } else {
    coefficients <- summary(model)$coefficients$cond
  }

  # Ensure the term exists in the model
  if (!term %in% rownames(coefficients)) {
    stop(paste("The term", term, "is not found in the model."))
  }

  # Extract values
  estimate <- coefficients[term, "Estimate"]
  se <- coefficients[term, "Std. Error"]

  if (inherits(model, "lmerModLmerTest")) {
    df <- coefficients[term, "df"]
    t_crit <- qt(1 - alpha, df = df)
  } else {
    df <- NULL
    t_crit <- NULL
  }
  # Calculate critical t-value

  # Return as a named list
  return(list(estimate = estimate, df = df, se = se, t_crit = t_crit))
}
