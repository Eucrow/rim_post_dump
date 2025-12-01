#' Change the content of variable TALL.PESO to TRUE.
#' This variable is T for lenghts samples and P for weight samples.
#' We allways work with lengths samples so all of them must be T.
#
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return Return a dataframe with the TALL.PESO variable fixed
fix_length_weight_variable <- function(lengths) {
  if ("TALL.PESO" %in% colnames(lengths)) {
    lengths[["TALL.PESO"]] <- "T"
    return(lengths)
  } else {
    stop(paste0("TALL.PESO doesn't exists in ", substitute(lengths)))
  }
}


# TO DO: this function is usefull?????
#' Detect variables which are not consistent with its SIRENO master
#' Check if the value of variables are consistent to the value in its SIRENO master.
#' It's only available for variables with a data source (master): ESTRATO_RIM, COD_PUERTO,
#' COD_ORIGEN, COD_ARTE, COD_PROCEDENCIA and TIPO_MUESTREO
#' @param df Data frame with RIM data
#' @param variable one of this values: ESTRATO_RIM, COD_PUERTO, COD_ORIGEN,
#' COD_ARTE, COD_PROCEDENCIA or TIPO_MUESTREO
#' @return A data frame with samples containing erroneous variables if found, NULL otherwise
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

#' Detect if a variable exists in a dataframe
#' TODO: IS THIS FUNCTION USEFULL???
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

#' TODO: IS THIS FUNCTION USEFULL???
#' Detect if a variable or variables of a dataframe contain empty values.
#' @param variables: vector with variables to check.
#' @param df: dataframe to check.
#' @param df_name: name of the dataframe where the error is found.
#' @return A list with a dataframe of every variable with empty values. Every
#' dataframe contains erroneous rows.
#' @export
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



#' Function to process length's RIM file for the functions checkMiddleMeasures() and
#' checkMeasures()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A processed dataframe to work with it in the mentioned functions
#' at the description
process_length_file_for_check_measures <- function(lengths) {
  # Work columns

  columns <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "COD_ESP_CAT",
    "COD_CATEGORIA",
    "TALLA"
  )

  lengths <- lengths[, columns]

  # Count the number of measurement registers

  lengths_register <- lengths %>%
    group_by(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_CAT) %>%
    mutate(REGISTROS = n_distinct(TALLA))

  # Count the number of half centimeter measurements done

  lengths_middle <- lengths %>%
    group_by(COD_ID, FECHA_MUE, COD_BARCO, COD_ESP_CAT) %>%
    filter(grepl("\\.5$", TALLA)) %>%
    mutate(TALLAS_MED = n_distinct(TALLA)) %>%
    select(-TALLA) %>%
    unique()

  # Merge both dataframes

  lengths <- merge(lengths_register, lengths_middle, all.x = TRUE)

  # Clean up NA values, make them zero

  lengths[is.na(lengths)] <- 0

  return(lengths)
}

#' Detech priority species with catches but not measured.
#' Use MT2A and MT2B samples.
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @param group character vector with priority group ("G1" to "G6")
#' @return Data frame with species of the priority group without samples measured
#' @note
#' This function does not return the errors column. Used in functions
#' g1_species_not_measured and g2SpeciesNotMeasured.
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
