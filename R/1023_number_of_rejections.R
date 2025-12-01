#' Detect number of rejects (only empty, nas or null)
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1023
number_of_rejections <- function(catches) {
  errors <- catches %>%
    filter(N_RECHAZOS == "" | is.na(N_RECHAZOS) | is.null(N_RECHAZOS)) %>%
    select(any_of(BASE_FIELDS)) %>%
    unique()
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: número de rechazos sin rellenar")
    return(errors)
  }
  
  return(NULL)
}
