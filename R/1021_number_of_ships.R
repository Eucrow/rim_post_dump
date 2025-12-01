#' Detect errors in number of ships (empty field, NA, 0 or > 2)
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1021
number_of_ships <- function(catches) {
  errors <- catches[
    catches["N_BARCOS"] == 0 |
      catches["N_BARCOS"] > 2 |
      is.null(catches["N_BARCOS"]) |
      is.na(catches["N_BARCOS"]),
    c(BASE_FIELDS, "N_BARCOS")
  ]
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: número de barcos igual a 0 o mayor de 2"
    )
    return(errors)
  }
  
  return(NULL)
}
