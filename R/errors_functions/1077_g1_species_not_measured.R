#' Detect G1 priority species with catches but not measured
#' @details Use MT2A and MT2B samples
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return Data frame with species of the priority group without samples measured
#' @note Check code: 1077
g1_species_not_measured <- function(catches, lengths) {
  errors <- priority_species_not_measured(catches, lengths, "G1")

  if (nrow(errors) != 0) {
    errors <- add_type_of_error(errors, "ERROR: especie prioritaria G1 no muestreada.")
    return(errors)
  }
  
  return(NULL)
}
