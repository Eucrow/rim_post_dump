# TODO: This function detects the checked samples, not the validated ones
#' Detect samples that are not validated
#' @details Column "VALIDADO" from length dataframe with FALSE or NA values
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return Data frame with errors if found, NULL otherwise
#' @note Check code: 1091
sampling_is_checked <- function(lengths){
  
  # Base work columns
  
  BASE_FIELDS <- c(BASE_FIELDS, "VALIDADO")
  
  # Filter FALSE and/or NA values from "VALIDADO" column
  
  lengths <- lengths[is.na(lengths$VALIDADO) | 
                       lengths$VALIDADO == FALSE, 
                     BASE_FIELDS]
  
  if(nrow(lengths)>0){
    lengths <- add_type_of_error(
      lengths,
      "WARNING: Muestra no chequeada."
    )
    return(lengths)
  }
  
  return(NULL)
}
