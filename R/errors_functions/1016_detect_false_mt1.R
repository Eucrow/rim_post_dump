#' Detect false mt1 samples
#' @details Samples with COD_TIPO_MUE as MT1A and lengths
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1016
detect_false_mt1 <- function(catches, lengths) {
  #Select all the samples with COD_TIPO_MUE = MT1

  mt1 <- catches[catches[["COD_TIPO_MUE"]] == 1, c(BASE_FIELDS)]
  mt1 <- unique(mt1)

  # select all the samples with lengths
  mt1_with_lenghts <- lengths %>%
    filter(COD_TIPO_MUE == 1) %>%
    group_by_at(BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))

  # check if all the samples keyed as MT1 hasn't lenghts
  false_mt1 <- merge(x = mt1, y = mt1_with_lenghts, by = BASE_FIELDS) %>%
    unique()
  
  if (nrow(false_mt1) > 0) {
    false_mt1 <- add_type_of_error(false_mt1, "ERROR: MT1 con tallas")
    return(false_mt1)
  }
  
  return(NULL)
}
