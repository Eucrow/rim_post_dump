# TO DO: this function is usefull?????
#' Detect variables which are not consistent with its SIRENO master
#' Check if the value of variables are consistent to the value in its SIRENO master.
#' It's only available for variables with a data source (master): ESTRATO_RIM, COD_PUERTO,
#' COD_ORIGEN, COD_ARTE, COD_PROCEDENCIA and TIPO_MUESTREO
#' @param df Data frame with RIM data
#' @param variable one of this values: ESTRATO_RIM, COD_PUERTO, COD_ORIGEN,
#' COD_ARTE, COD_PROCEDENCIA or TIPO_MUESTREO
#' @return A data frame with samples containing erroneous variables if found, NULL otherwise
#' @note Check code: 1092
variable_not_in_master <- function(df, variable) {
  if (
    variable != "ESTRATO_RIM" &&
      variable != "COD_PUERTO" &&
      variable != "COD_ORIGEN" &&
      variable != "COD_ARTE" &&
      variable != "PROCEDENCIA" &&
      variable != "COD_TIPO_MUE"
  ) {
    stop(paste("This function is not available for ", variable))
  }

  # If the variable begin with "COD_", the name of the data source
  # is the name of the variable without "COD_"
  variable_formatted <- variable
  if (grepl("^COD_", variable)) {
    variable_formatted <- strsplit(variable, "COD_")
    variable_formatted <- variable_formatted[[1]][2]
  }

  # In case COD_TIPO_MUE, the data set is "tipo_muestreo" instead of "tipo_mue":
  if (variable_formatted == "TIPO_MUE") {
    name_dataset <- "tipo_muestreo"
  } else {
    name_dataset <- tolower(variable_formatted)
  }

  #search the errors in variable
  errors <- anti_join(df, get(name_dataset), by = variable)

  #prepare to return
  fields_to_filter <- c(BASE_FIELDS, variable, variable_formatted)

  errors <- errors %>%
    select(any_of(fields_to_filter)) %>%
    unique()

  if (nrow(errors) > 0) {
    text_type_of_error <- paste0(
      "ERROR: el campo ",
      variable_formatted,
      " no concuerda con los maestros de este script."
    )
    errors <- add_type_of_error(errors, text_type_of_error)
    #return
    return(errors)
  }
  
  return(NULL)
}
