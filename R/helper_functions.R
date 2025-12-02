#' Detect if a variable exists in a dataframe
#' @return TRUE if the variable exists. Otherwise return an error.
#' @param variable: variable to check.
#' @param df: dataframe to check
#' @export
variable_exists_in_df <- function(variable, df) {
  # get all the variables of df with the variable name = variable
  var_in_df <- colnames(df)[colnames(df) %in% variable]

  # TO DO: THE FIRST CONDITIONAL IS NOT NEEDED, BECAUSE COLUMS NAMES
  # ARE UNIQUE IN A DATAFRAME. CHECK IT AND REMOVE IT IF IT'S TRUE.
  if (length(var_in_df) > 1) {
    stop(paste(
      "Hey hard worker! check the ",
      variable,
      "variable. Looks like there are multiple columns with the same variable name.
               Using consistents dataframes we will get a better world. Really :) ",
      variable,
      "."
    ))
  } else if (length(var_in_df) == 0) {
    stop(paste(variable, " does not exists in this dataframe."))
  } else return(TRUE)
}

#' Detect empty values in specified dataframe variables
#'
#' @description
#' Checks for empty or NA values in specified variables of a RIM dataframe.
#' Returns rows with errors, formatted with appropriate fields based on variable type.
#'
#' @param df Data frame. The dataframe to check (catches or lengths)
#' @param variables Character vector. Names of variables to check for empty values
#' @param df_name Character string. Name of the dataframe/screen where errors were found
#'   (used in error messages). Can be empty string
#'
#' @return Data frame or NULL. If errors found, returns a merged dataframe with all
#'   erroneous rows including TIPO_ERROR column. Returns NULL if no errors found
#'
#' @details
#' Special handling for specific variable types:
#' \itemize{
#'   \item BASE_FIELDS variables: Returns only BASE_FIELDS + TIPO_ERROR
#'   \item EJEM_MEDIDOS, EJEM_PONDERADOS, SOP: Includes species and category details
#'   \item P_MUE_VIVO, P_MUE_DESEM: Includes species and category info without TALLA
#' }
empty_values_in_variables <- function(df, variables, df_name) {
  # check if all the variables are in the dataframe
  if (!all(variables %in% colnames(df))) {
    stop("Not all the variables are in the dataframe.")
  }

  variables <- as.list(variables)

  if (df_name != "") {
    df_name <- paste(" in", df_name, " screen")
  }

  errors <- lapply(variables, function(x) {
    error <- (df[df[[x]] == "" | is.na(df[[x]]), ])

    if (nrow(error) > 0) {
      error <- add_type_of_error(error, "ERROR: Variable ", x, " vacía", df_name)
      if (x %in% BASE_FIELDS) {
        error <- error[, c(BASE_FIELDS, "TIPO_ERROR")]
      } else {
        # The variables "EJEM_MEDIDOS", "EJEM_PONDERADOS", "SOP"
        # and "P_MUE_DESEM", must have more
        # variables to properly identify the error:
        if (x %in% c("EJEM_MEDIDOS", "EJEM_PONDERADOS", "SOP")) {
          error <- error[, c(
            BASE_FIELDS,
            "COD_ESP_MUE",
            "ESP_MUE",
            "COD_CATEGORIA",
            "CATEGORIA",
            "COD_ESP_CAT",
            "ESP_CAT",
            "TALLA",
            x,
            "TIPO_ERROR"
          )]
        } else if (x %in% c("P_MUE_VIVO", "P_MUE_DESEM")) {
          # The variables"P_MUE_DESEM", must have more
          # variables to properly identify the error:
          error <- error[, c(
            BASE_FIELDS,
            "COD_ESP_MUE",
            "ESP_MUE",
            "COD_CATEGORIA",
            "CATEGORIA",
            "COD_ESP_CAT",
            "ESP_CAT",
            x,
            "TIPO_ERROR"
          )]
        } else {
          error <- error[, c(BASE_FIELDS, x, "TIPO_ERROR")]
        }
      }
    }
    return(error)
  })

  errors <- lapply(errors, unique)

  errors <- Filter(function(x) nrow(x) > 0, errors)

  if (length(errors) > 0) {
    errors <- Reduce(
      function(x, y) {
        merge(x, y, all = TRUE)
      },
      errors
    )

    return(errors)
  }
}

#' Detect priority species with catches but not measured.
#' Use MT2A and MT2B samples.
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @param group character vector with priority group ("G1" to "G6")
#' @return Data frame with species of the priority group without samples measured
#' @note
#' This is a helper function used by \code{g1_species_not_measured()} and
#' \code{g2_species_not_measured()} check functions.
priority_species_not_measured <- function(catches, lengths, group) {
  sps <- unique(especies_prioritarias[
    especies_prioritarias$PRIORIDAD %in% group,
    "COD_ESP_MUE"
  ])

  catches <- catches[
    catches$COD_TIPO_MUE %in% c(2, 4),
    c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")
  ]
  catches <- unique(catches)

  lengths <- lengths[
    lengths$COD_TIPO_MUE %in% c(2, 4),
    c(
      BASE_FIELDS,
      "COD_ESP_MUE",
      "ESP_MUE",
      "COD_ESP_CAT",
      "ESP_CAT",
      "EJEM_MEDIDOS"
    )
  ]
  lengths <- lengths[lengths$COD_ESP_MUE %in% sps, ]

  lengths <- lengths[!is.na(lengths$EJEM_MEDIDOS), ]
  lengths <- lengths[lengths$EJEM_MEDIDOS != 0, ]
  lengths$EJEM_MEDIDOS <- "T"
  lengths <- unique(lengths)
  lengths <- lengths[, c(
    "COD_ID",
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_ESP_CAT",
    "ESP_CAT",
    "EJEM_MEDIDOS"
  )]

  catches <- catches[catches$COD_ESP_MUE %in% sps, ]

  errors <- merge(
    catches,
    lengths,
    by = c("COD_ID", "COD_ESP_MUE", "ESP_MUE"),
    all.x = TRUE
  )

  errors <- errors[is.na(errors$EJEM_MEDIDOS) | errors$EJEM_MEDIDOS == 0, ]

  errors <- errors[, c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_ESP_CAT",
    "ESP_CAT"
  )]
}

#' Prepare RIM length data for measurement validation checks
#'
#' @description
#' Processes length data to count total measurements and half-centimeter measurements
#' per species sample, preparing it for use in measurement validation functions.
#'
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#'
#' @return Data frame. Processed lengths data with two additional columns:
#'   REGISTROS: Count of distinct size measurements per species sample
#'   TALLAS_MED: Count of half-centimeter measurements (ending in .5) per sample
#'
#' @note
#' Used by check functions 1076 (incorrect_half_cm_measures) and
#' 1084 (incorrect_cm_measures).
prepare_lengths_for_check_measures <- function(lengths) {
  lengths <- lengths[, c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "TALLA"
  )]

  # Count the distinct size of every species sample
  lengths_register <- lengths %>%
    group_by(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_CAT) %>%
    mutate(REGISTROS = n_distinct(TALLA))

  # Count the number of size entries ending in .5 (0.5 cm measurements)
  lengths_middle <- lengths %>%
    group_by(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_CAT) %>%
    filter(grepl("\\.5$", TALLA)) %>%
    mutate(TALLAS_MED = n_distinct(TALLA)) %>%
    select(-TALLA) %>%
    unique()

  # Merge both dataframes
  lengths <- merge(lengths_register, lengths_middle, all.x = TRUE)

  lengths[is.na(lengths)] <- 0

  return(lengths)
}

