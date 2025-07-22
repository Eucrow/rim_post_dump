#' Check code: 1088
#' Check if the sampled species are present in the historical dataset from 2021 to 2024
#' @param catches_in_lengths: catches data frame returned by the importRIMCatchesInLengths() 
#' or importRIMFiles() functions.
#' @return dataframe with erroneous samples

checkNewSpeciesSampled <- function(catches_in_lengths){
  
  error <- catches_in_lengths[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", 
                                  "COD_ESP_CAT", "ESP_CAT")]
  
  historical_species_sampled$TEST <- "T"
  
  error <- merge(error,
                 historical_species_sampled,
                 all.x = TRUE)
  
  error <- error[is.na(error_test$TEST), ]
  
  if(nrow(error)>0){
    error <- addTypeOfError(error,
                            "WARNING 1088: especie no muestreada en el histórico 2021-2024")
  }
  
  return(error)
  
}