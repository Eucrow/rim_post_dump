#' Detect foreign ships in MT2 samples
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1024
foreign_ships_mt2 <- function(catches) {
  ships <- catches %>%
    select(any_of(BASE_FIELDS)) %>%
    filter(grepl("^8\\d{5}", COD_BARCO), COD_TIPO_MUE == "2") %>%
    unique()

  if (nrow(ships) > 0) {
    ships <- add_type_of_error(ships, "WARNING: MT2 con barco extranjero")
    return(ships)
  }
  
  return(NULL)
}
