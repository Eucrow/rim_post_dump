#' Check code: 1051
#' Check samples with same date, vessel, ESTRATO_RIM and TIPO_MUE, and gear but with different
#' port variable.
#' @param catches: catches data frame returned by the importRIMCatchesInLengths() 
#' or importRIMFiles() functions.
#' @return dataframe with erroneous samples

checkSameHaulsVariablesDifferentPorts <- function(catches) {
  
  error <- catches[, BASE_FIELDS] %>% 
    unique() %>% 
    group_by(FECHA_MUE, COD_BARCO, ESTRATO_RIM, COD_TIPO_MUE) %>%
    mutate(DIST_PORTS = n_distinct(COD_PUERTO)) %>%
    filter(DIST_PORTS != 1)
  
  
  if(nrow(error) > 1){
    error <- addTypeOfError(error,
                            "ERROR 1051: mismo fecha/barco/estrato_rim/tipo_muestreo con distinto PUERTO")
  }
  
  return(errors)
  
}