#' Detect if the content of variable TALL.PESO is allways TRUE
#' @details This logical variable is TRUE for lenghts samples and false to
#' weight samples.
#' We allways work with lengths samples so all of them must be TRUE
#' @param catches Data frame with catches data
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1046
validate_length_weight_variable <- function(catches) {
    errors <- catches %>%
      select(any_of(c(BASE_FIELDS, "TALL.PESO"))) %>%
      filter(TALL.PESO != "T" | is.na(TALL.PESO) | is.null(TALL.PESO)) %>%
      unique()
    
    if (nrow(errors) > 0) {
      errors <- add_type_of_error(errors, "ERROR: Muestreo no metido como muestreo de talla")
      return(errors)
    }
    
    return(NULL)
}
