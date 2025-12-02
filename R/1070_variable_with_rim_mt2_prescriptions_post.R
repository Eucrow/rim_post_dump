#' Detect if a variable is not in MT2 rim prescriptions
#' @details Use the prescriptions_rim_2021_variables dataset
#' @param df Dataframe where the variable to check is
#' @param variable Variable to check as character. Allowed variables:
#' ESTRATO_RIM, COD_PUERTO, COD_ORIGEN, COD_ARTE, METIER_DCF and CALADERO_DCF
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1070
variable_with_rim_mt2_prescriptions_post <- function(df, variable) {
  valid_variables = c(
    "ESTRATO_RIM",
    "COD_PUERTO",
    "COD_ORIGEN",
    "COD_ARTE",
    "METIER_DCF",
    "CALADERO_DCF"
  )
  if (!(variable %in% valid_variables)) {
    stop(paste("This function is not available for variable", variable))
  }

  allowed <- prescripciones_rim_mt2_coherencia[, variable]

  # only MT2 samples:
  df <- df[df[["COD_TIPO_MUE"]] == 2, ]

  df <- df[!(df[[variable]] %in% allowed), ]

  if (nrow(df) > 0) {
    fields <- BASE_FIELDS

    if (!(variable %in% BASE_FIELDS)) {
      fields <- c(BASE_FIELDS, variable)
    }

    df <- df[, fields]

    df <- unique(df)

    df[["TIPO_ERROR"]] <- apply(df, 1, function(x) {
      paste0(
        "La variable ",
        variable,
        " ",
        x[[variable]],
        " no está en las prescripciones MT2 actuales."
      )
    })

    return(df)
  }
  
  return(NULL)
}
