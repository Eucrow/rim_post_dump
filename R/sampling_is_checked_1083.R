#' Check code: 1083
#' Detect samples that are not validated (column "VALIDADO" from length dataframe
#' with FALSE or NA values).
#' @param lengths: catches data frame returned by the importRIMCatchesInLengths() 
#' or importRIMFiles() functions.
#' @return Data frame whit errors.

sampling_is_checked <- function(lengths){
  
  # Base work columns
  
  BASE_FIELDS <- c(BASE_FIELDS, "VALIDADO")
  
  # Filter FALSE and/or NA values from "VALIDADO" column
  
  lengths <- lengths[is.na(lengths$VALIDADO) | 
                       lengths$VALIDADO == FALSE, 
                     BASE_FIELDS]
  
  if(nrow(lengths)>0){
    lengths <- addTypeOfError(
      lengths,
      "WARNING: sample is not checked"
    )
    return(lengths)
  }
}