#' Detect false mt2 samples
#' @details Samples with COD_TIPO_MUE as MT2 and without any length
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1017
detect_false_mt2 <- function(catches, lengths) {
  #Select all the samples with COD_TIPO_MUE = MT2
  mt2 <- catches[
    catches[["COD_TIPO_MUE"]] == 2 | catches[["COD_TIPO_MUE"]] == 4,
    c(BASE_FIELDS)
  ]
  mt2 <- unique(mt2)

  # select all the samples with lengths
  mt2_with_lenghts <- lengths %>%
    filter(COD_TIPO_MUE == 2 | COD_TIPO_MUE == 4) %>%
    group_by_at(BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))

  # check if all the samples keyed as MT2 has lengths
  false_mt2 <- anti_join(
    x = mt2,
    y = mt2_with_lenghts,
    by = c("FECHA_MUE", "COD_BARCO")
  ) %>%
    unique()
  
  if (nrow(false_mt2) > 0) {
    false_mt2 <- add_type_of_error(false_mt2, "ERROR: MT2 sin tallas")
    return(false_mt2)
  }
  
  return(NULL)
}
