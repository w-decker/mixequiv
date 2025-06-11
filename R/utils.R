
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

  if (!inherits(model, "lmerModLmerTest")) {
    stop("The argument 'model' must be of class 'lmerModLmerTest'.")
  }

  # Extract summary coefficients
  coefficients <- summary(model)$coefficients

  # Ensure the term exists in the model
  if (!term %in% rownames(coefficients)) {
    stop(paste("The term", term, "is not found in the model."))
  }

  # Extract values
  estimate <- coefficients[term, "Estimate"]
  df <- coefficients[term, "df"]
  se <- coefficients[term, "Std. Error"]

  # Calculate critical t-value
  t_crit <- qt(1 - alpha, df = df)

  # Return as a named list
  return(list(estimate = estimate, df = df, se = se, t_crit = t_crit))
}

is.uppercase <- function(x) {
  #' Detects if all characters in a character are uppercase
  #'
  #' @param x character
  #' @return boolean
  #' @export

  if (!all(stringr::str_detect(unlist(strsplit(x, "")), "[[:upper:]]"))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
