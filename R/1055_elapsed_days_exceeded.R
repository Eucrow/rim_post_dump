#' Detect elapsed days between landing date and sampling date greater
#' than 3 or minor than 0
#' @param catches Data frame with catches data
#' @return A data frame with erroneous trips if found, NULL otherwise
#' @note Check code: 1055
elapsed_days_exceeded <- function(catches) {
  catches$FECHA_MUE <- as.POSIXlt(catches$FECHA_MUE, format = "%d-%m-%y")

  # change the column "FECHA_DESEM" to a date format
  # to avoid some problems with Spanish_Spain.1252 (or if you are using another
  # locale), change locale to Spanish_United States.1252:
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "Spanish_United States.1252")

  catches[["FECHA_DESEM"]] <- as.POSIXlt(
    catches[["FECHA_DESEM"]],
    format = "%d-%b-%y"
  )

  # and now the come back to the initial configuration of locale:
  Sys.setlocale("LC_TIME", lct)

  catches[["elapsed_days"]] <- difftime(
    catches[["FECHA_MUE"]],
    catches[["FECHA_DESEM"]],
    units = "days"
  )

  catches[["FECHA_MUE"]] <- as.POSIXct(catches[["FECHA_MUE"]])
  catches[["FECHA_DESEM"]] <- as.POSIXct(catches[["FECHA_DESEM"]])

  errors <- catches %>%
    select(any_of(c(BASE_FIELDS, "FECHA_DESEM", "elapsed_days"))) %>%
    filter(elapsed_days > 3 | elapsed_days < (-1)) %>%
    unique()

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: tiempo transcurrido entre la fecha de desembarco y la de muestreo mayor que 3 días o menor que 0 días"
    )
    return(errors)
  }
  
  return(NULL)
}
