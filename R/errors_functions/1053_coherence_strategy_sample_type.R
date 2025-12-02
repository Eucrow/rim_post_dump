#' Detect incoherence between ESTRATEGIA and COD_TIPO_MUE
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1053
coherence_strategy_sample_type <- function(catches) {
  error_strategy <- catches %>%
    select(any_of(c(BASE_FIELDS, "ESTRATEGIA"))) %>%
    anti_join(y = tipo_muestreo, by = c("COD_TIPO_MUE", "ESTRATEGIA"))

  # MT2 samples of VORACERA_GC must be "En base a especie", so remove it of
  # the errors dataframe
  error_strategy <- error_strategy[
    -which(
      error_strategy$ESTRATO_RIM == "VORACERA_GC" &
        error_strategy$ESTRATEGIA == "En base a especie"
    ),
  ]

  if (nrow(error_strategy) > 0) {
    error_strategy <- add_type_of_error(
      error_strategy,
      "ERROR: No concuerda el campo ESTRATEGIA con el campo TIPO DE MUESTREO"
    )
    return(error_strategy)
  }
  
  return(NULL)
}
