#' Detect foreign ships in MT1 samples
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1089
foreign_ships_mt1 <- function(catches) {
  ships <- catches %>%
    select(any_of(BASE_FIELDS)) %>%
    filter(grepl("^8\\d{5}", COD_BARCO), COD_TIPO_MUE == "1") %>%
    unique()

  if (nrow(ships) > 0) {
    ships <- add_type_of_error(ships, "ERROR: MT1 con barco extranjero")
    return(ships)
  }
  
  return(NULL)
}
