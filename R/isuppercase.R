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
