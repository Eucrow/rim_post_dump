#' Detect trips with the same COD_BARCO and FECHA_MUE but different COD_TIPO_MUE
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1054
multiple_type_sample <- function(catches) {
  error <- catches %>%
    select(COD_ID, COD_PUERTO, COD_BARCO, FECHA_MUE, COD_TIPO_MUE) %>%
    unique() %>%
    group_by(COD_BARCO, FECHA_MUE) %>%
    mutate(number_type_sample = n_distinct(COD_TIPO_MUE, COD_BARCO)) %>%
    filter(number_type_sample > 1)

  if (nrow(error) > 0) {
    error <- error %>%
      add_type_of_error(
        "ERROR: Para un mismo barco y fecha, hay muestreos de varios tipos"
      )
    error <- humanize(error)
    return(error)
  }
  
  return(NULL)
}
