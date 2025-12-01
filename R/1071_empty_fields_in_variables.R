#' TODO: IS THIS FUNCTION USEFULL???
#' Empty fields in variables
#' @details Return empty variables of a RIM dataframes imported by importRIM functions. Only
#' variables saved in formato_variables dataset as mandatory are checked. Require one of the dataframes returned by importRIMFiles functions:
#' importRIMCatches() and importRIMLengths()
#' @param df dataframe returned by one of the importRIM functions
#' @param type_file type of the imported file according to this values:
#' RIM_CATCHES or RIM_LENGTHS
#' @return A dataframe with the COD_MAREA and variables with values missing
#' @note Check code: 1071
empty_fields_in_variables <- function(
  df,
  type_file = c("RIM_CATCHES", "RIM_LENGTHS")
) {
  # Detect if the variable type_file is correct:
  match.arg(type_file)

  # Create helper_text
  helper_text <- substr(type_file, 5, nchar(type_file))
  helper_text <- tolower(helper_text)

  mandatory_field <- paste0(type_file, "_MANDATORY")

  mandatory <- formato_variables[
    which(
      !is.na(formato_variables[type_file]) &
        formato_variables[mandatory_field] == TRUE
    ),
    c("name_variable")
  ]
  df_mandatory <- df[, mandatory]

  err <- empty_values_in_variables(df_mandatory, mandatory, helper_text)

  # in case there aren't any errors, check_empty_values returns NULL, so:
  if (!is.null(err)) {
    return(err)
  }
  
  return(NULL)
}
