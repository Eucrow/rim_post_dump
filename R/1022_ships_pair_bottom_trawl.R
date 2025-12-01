#' Detect ESTRATO_RIM and gear of trips with 2 ships
#' @details Exception with Santa Eugenia de Ribeira port, that usually only one ship is
#' sampled
#' @param catches Data frame with catches data
#' @return A data frame with erroneous trips if found, NULL otherwise
#' @note Check code: 1022
ships_pair_bottom_trawl <- function(catches) {
  errors <- catches %>%
    select(any_of(BASE_FIELDS), N_BARCOS) %>%
    unique() %>%
    filter(COD_PUERTO != "0917") %>% #This is an execption: only one ship is sampled in Ribeira pair bottom trails
    filter(
      ((N_BARCOS == 2 | N_BARCOS == 3 | N_BARCOS == 4) &
        ESTRATO_RIM != "PAREJA_CN") |
        (N_BARCOS != 2 &
          N_BARCOS != 3 &
          N_BARCOS != 4 &
          ESTRATO_RIM == "PAREJA_CN")
    )
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: número de barcos no coherente con el estrato rim")
    return(errors)
  }
  
  return(NULL)
  #errors <- addInfluenceAreaVariable(errors, "COD_PUERTO")
  #write.csv(errors, file = "parejas_num_barcos_1.csv")
}
