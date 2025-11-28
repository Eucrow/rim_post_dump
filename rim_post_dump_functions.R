#' Detect coherence between rim stratum and gear
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param specification Specification parameter
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1019
coherence_rim_stratum_gear <- function(catches, specification) {
  estrato_rim_arte$VALID <- TRUE

  catches <- merge(
    x = catches,
    y = estrato_rim_arte,
    by.x = c("ESTRATO_RIM", "COD_ARTE", "ARTE"),
    by.y = c("ESTRATO_RIM", "COD_ARTE", "ARTE"),
    all.x = TRUE
  )

  errors <- catches[is.na(catches$VALID), ]

  if (nrow(errors) > 0) {
    errors <- errors[, c(BASE_FIELDS, "COD_ARTE", "ARTE")]
    errors <- unique(errors)
    errors <- add_type_of_error(
      errors,
      "ERROR: el arte no es coherente con el estrato rim."
    )

    return(errors)
  }
  
  return(NULL)
}


#' Detect errors in number of ships (empty field, = 0 or > 2)
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1021
number_of_ships <- function(catches) {
  errors <- catches[
    catches["N_BARCOS"] == 0 |
      catches["N_BARCOS"] > 2 |
      is.null(catches["N_BARCOS"]),
    c(BASE_FIELDS, "N_BARCOS")
  ]
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: número de barcos igual a 0 o mayor de 2"
    )
    return(errors)
  }
  
  return(NULL)
}


#' Detect number of rejects (only empty, nas or null)
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1023
number_of_rejections <- function(catches) {
  errors <- catches %>%
    filter(N_RECHAZOS == "" | is.na(N_RECHAZOS) | is.null(N_RECHAZOS)) %>%
    select(any_of(BASE_FIELDS)) %>%
    unique()
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: número de rechazos sin rellenar")
    return(errors)
  }
  
  return(NULL)
}


#' Detect samples with SOP > P_MUE_VIVO
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1043
sop_greater_pes_mue_vivo <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_VIVO",
    "SOP"
  )
  errors <- catches_in_lengths %>%
    select(any_of(selected_fields)) %>%
    mutate(DIF_SOP_P_MUE_VIVO = SOP - P_MUE_VIVO) %>%
    mutate(DIF_SOP_P_MUE_VIVO = round(DIF_SOP_P_MUE_VIVO, 2)) %>%
    filter(DIF_SOP_P_MUE_VIVO > 0.01)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: SOP mayor que peso muestreado vivo")
    return(errors)
  }
  
  return(NULL)
}


#' Detect samples with SOP = 0
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1042
sop_zero <- function(catches_in_lengths) {
  fields_to_select <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_DESEM",
    "SOP"
  )
  errors <- catches_in_lengths %>%
    select(any_of(fields_to_select)) %>%
    filter(SOP == 0)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: SOP igual a 0")
    return(errors)
  }
  
  return(NULL)
}

#' Detect samples with P_MUE_DESEM = 0 or NA
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1040
pes_mue_desem_zero <- function(catches_in_lengths) {
  fields_to_select <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_DESEM"
  )
  errors <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0 | is.na(P_MUE_DESEM)) %>%
    select(any_of(fields_to_select))

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: Peso muestreado es 0")
    return(errors)
  }
  
  return(NULL)
}


#' Detect samples with p_desem <= p_mue_desem
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1045
pes_mue_desem_greater_pes_desem <- function(catches_in_lengths) {
  fields_to_select <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_DESEM",
    "P_MUE_DESEM",
    "DIF_P_MUE_DESEM_P_DESEM"
  )

  errors <- catches_in_lengths %>%
    mutate(DIF_P_MUE_DESEM_P_DESEM = P_MUE_DESEM - P_DESEM) %>%
    filter(DIF_P_MUE_DESEM_P_DESEM > 0) %>%
    select(any_of(fields_to_select)) %>%
    unique()

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: Peso muestreado mayor al peso desembarcado"
    )
    return(errors)
  }
  
  return(NULL)
}


#' Detect samples with SOP > P_VIVO
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1044
sop_greater_pes_vivo <- function(catches_in_lengths) {
  fields_to_select <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "SOP",
    "P_VIVO",
    "DIF_SOP_P_VIVO"
  )

  errors <- catches_in_lengths %>%
    mutate(DIF_SOP_P_VIVO = SOP - P_VIVO) %>%
    mutate(DIF_SOP_P_VIVO = round(DIF_SOP_P_VIVO, 2)) %>%
    filter(DIF_SOP_P_VIVO > 0.01) %>%
    select(any_of(fields_to_select))

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: SOP mayor que peso vivo desembarcado"
    )
    return(errors)
  }
  
  return(NULL)
}


#' Detect categories with equal species weight landings
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1041
species_with_categories_with_same_weight_landing <- function(catches) {
  catches <- catches[, c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "P_DESEM"
  )]
  fields_to_count <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM")
  errors <- catches %>%
    unique() %>%
    group_by_at(c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "P_DESEM")) %>%
    summarise(n = n()) %>%
    filter(n > 1)
  
  if (nrow(errors) > 0) {
    colnames(errors)[names(errors) == "n"] <- "NUM_OCU_CAT_MISMO_PESO_DESEM"
    errors <- add_type_of_error(
      errors,
      "WARNING: varias categorías con igual peso desembarcado"
    )
    return(errors)
  }
  
  return(NULL)
}

#' Detect samples with categories which all of this species has the same sampled weight
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1036
all_categories_with_same_sampled_weights <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "N_CATEGORIAS",
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "P_MUE_DESEM",
    "SEXO"
  )

  errors <- catches_in_lengths %>%
    group_by_at(selected_fields) %>%
    summarise(NUM_ESP_CAT_MISMO_PESO_MUE = n()) %>%
    filter(NUM_ESP_CAT_MISMO_PESO_MUE > 1)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: varias especies de la categorías con igual peso muestreado"
    )
    return(errors)
  }
  
  return(NULL)
}

#' Detect samples with doubtful species of the category where
#' the genus finished in -formes, -dae, - spp and - sp
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1031
doubtful_category_species <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT"
  )

  #create a dataframe with other species not allowed
  # by sufixes
  to_check_genus <- grep(
    "(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",
    catches_in_lengths$ESP_CAT
  )

  genus_not_allowed <- catches_in_lengths[to_check_genus, ] %>%
    select(any_of(selected_fields))
  errors <- unique(genus_not_allowed)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: ¿seguro que es esa especie en Especies de la Categoría?"
    )
    return(errors)
  }
  
  return(NULL)
}

#' Detect samples with not allowed species of the category
#' @details This function use the not allowed species dataframe of SAPMUEBASE
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1030
not_allowed_category_species <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT"
  )

  # create a dataframe with species not allowed
  not_allowed <- merge(
    x = catches_in_lengths,
    y = NOT_ALLOWED_SPECIES,
    by.x = "COD_ESP_CAT",
    by.y = "COD_ESP"
  ) %>%
    select(any_of(selected_fields))

  # remove duplicates
  errors <- unique(not_allowed)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: muestreos con especies no permitidos en Especies de la Categor?a"
    )
    return(errors)
  }
  
  return(NULL)
}


#' Detect foreign ships in MT1 samples
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1089
foreign_ships_mt1 <- function(catches) {
  ships <- catches %>%
    select(any_of(BASE_FIELDS)) %>%
    filter(grepl("^8\\d{5}", COD_BARCO), COD_TIPO_MUE == "1") %>%
    unique()

  if (nrow(ships) > 0) {
    ships <- add_type_of_error(ships, "ERROR: MT1 con barco extranjero")
    return(ships)
  }
  
  return(NULL)
}

#' Detect foreign ships in MT2 samples
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1024
foreign_ships_mt2 <- function(catches) {
  ships <- catches %>%
    select(any_of(BASE_FIELDS)) %>%
    filter(grepl("^8\\d{5}", COD_BARCO), COD_TIPO_MUE == "2") %>%
    unique()

  if (nrow(ships) > 0) {
    ships <- add_type_of_error(ships, "WARNING: MT2 con barco extranjero")
    return(ships)
  }
  
  return(NULL)
}


#' Ships registered in CFPO in removal state ("Baja Definitiva" and "Baja
#' Provisional")
#' @details Require a valid CFPO file.
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param cfpo: valid CFPO file.
#' @return A dataframe with errors.
#' @note Check code: 1026
ships_not_registered <- function(catches, cfpo = CFPO) {
  catches <- unique(catches[, c(BASE_FIELDS, "COD_SGPM")])
  catches[, "CFR"] <- sprintf("ESP%09d", as.numeric(as.character(catches[["COD_SGPM"]])))

  errors <- merge(x = catches, y = cfpo, by.x = "CFR", by.y = "CFR", all.x = TRUE)

  errors <- errors[
    errors[["ESTADO"]] %in% c("Baja Definitiva", "Baja Provisional"),
  ]

  if (nrow(errors) > 0) {
    text_type_of_error <- paste0(
      "ERROR: barco en ",
      errors[["ESTADO"]],
      " en el CFPO."
    )
    errors <- cbind(errors, "TIPO_ERROR" = text_type_of_error)
    return(errors)
  }
  
  return(NULL)
}


#' Ships not in CFPO
#' @details Require a valid CFPO file.
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param cfpo: valid CFPO file.
#' @return A dataframe with errors.
#' @note Check code: 1025
ships_not_in_cfpo <- function(catches, cfpo = CFPO) {
  catches <- unique(catches[, c(BASE_FIELDS, "COD_SGPM")])

  catches[, "CFR"] <- sprintf("ESP%09d", as.numeric(as.character(catches[["COD_SGPM"]])))

  errors <- merge(x = catches, y = cfpo, by.x = "CFR", by.y = "CFR", all.x = TRUE)

  errors <- errors[is.na(errors[["ESTADO"]]), ]

  # if(nrow(errors)>0) {
  #   errors <- add_type_of_error(errors, "ERROR: barco no incluido en el CFPO")
  #   return (errors)
  # }

  if (nrow(errors) > 0) {
    text_type_of_error <- paste0("ERROR: barco no incluido en el CFPO.")
    errors <- cbind(errors, "TIPO_ERROR" = text_type_of_error)
    return(errors)
  }
  
  return(NULL)
}

#' Detect if all mt2b has CERCO_GC or BACA_GC strata
#' @details Only useful in COD_TIPO_MUE = 4
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1018
validate_mt2b_rim_stratum <- function(catches) {
  # select all the mt2b samples

  mt2b <- catches[catches[["COD_TIP_MUE"]] == 4, c(BASE_FIELDS)]

  err_stratum <- mt2b[!(mt2b[["ESTRATO_RIM"]] %in% c("CERCO_GC", "BACA_GC")), ]

  err_stratum <- unique(err_stratum)

  if (nrow(err_stratum) > 0) {
    err_stratum <- add_type_of_error(
      err_stratum,
      "ERROR: MT2B con estrato rim distinto a CERCO_GC y BACA_GC"
    )
    return(err_stratum)
  }
  
  return(NULL)
}

#' Detect samples with weight sampled = 0 with lengths
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1037
weight_sampled_zero_with_lengths_sampled <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "P_DESEM",
    "P_VIVO",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_DESEM",
    "P_MUE_VIVO",
    "SOP"
  )
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM == 0) %>%
    select(any_of(selected_fields))
  
  if (nrow(err) > 0) {
    err <- add_type_of_error(err, "ERROR: peso muestra 0 con tallas muestreadas")
    return(err)
  }
  
  return(NULL)
}

#' Detect samples with weight landed = 0 or NA
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1038
weight_landed_zero <- function(catches) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "P_DESEM",
    "P_VIVO"
  )
  errors <- catches %>%
    filter(P_DESEM == 0 | is.na(P_DESEM))
  
  if (nrow(errors) > 0) {
    errors %>%
    select(any_of(selected_fields)) %>%
    add_type_of_error("ERROR: peso desembarcado = 0")
    return(errors)
  }
  
  return(NULL)
}

#' Detect samples without lengths but with weight sampled
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1039
weight_sampled_without_lengths_sampled <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "P_DESEM",
    "P_VIVO",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_DESEM",
    "EJEM_MEDIDOS"
  )
  err <- catches_in_lengths %>%
    filter(P_MUE_DESEM != 0 & (EJEM_MEDIDOS == 0 | is.na(EJEM_MEDIDOS)))
  
  if (nrow(err) > 0) {
    err %>%
    select(any_of(selected_fields)) %>%
    add_type_of_error(
      "ERROR: especie sin tallas muestreadas pero con peso muestra"
    )
    return(err)
  }
  
  return(NULL)
}


#' Detect false mt2 samples
#' @details Samples with COD_TIPO_MUE as MT2 and without any length
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1017
detect_false_mt2 <- function(catches, lengths) {
  #Select all the samples with COD_TIPO_MUE = MT2
  mt2 <- catches[
    catches[["COD_TIPO_MUE"]] == 2 | catches[["COD_TIPO_MUE"]] == 4,
    c(BASE_FIELDS)
  ]
  mt2 <- unique(mt2)

  # select all the samples with lengths
  mt2_with_lenghts <- lengths %>%
    filter(COD_TIPO_MUE == 2 | COD_TIPO_MUE == 4) %>%
    group_by_at(BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))

  # check if all the samples keyed as MT2 has lengths
  false_mt2 <- anti_join(
    x = mt2,
    y = mt2_with_lenghts,
    by = c("FECHA_MUE", "COD_BARCO")
  ) %>%
    unique()
  
  if (nrow(false_mt2) > 0) {
    false_mt2 <- add_type_of_error(false_mt2, "ERROR: MT2 sin tallas")
    return(false_mt2)
  }
  
  return(NULL)
}

#' Detect false mt1 samples
#' @details Samples with COD_TIPO_MUE as MT1A and lengths
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1016
detect_false_mt1 <- function(catches, lengths) {
  #Select all the samples with COD_TIPO_MUE = MT1

  mt1 <- catches[catches[["COD_TIPO_MUE"]] == 1, c(BASE_FIELDS)]
  mt1 <- unique(mt1)

  # select all the samples with lengths
  mt1_with_lenghts <- lengths %>%
    filter(COD_TIPO_MUE == 1) %>%
    group_by_at(BASE_FIELDS) %>%
    summarise(summatory = sum(EJEM_MEDIDOS, na.rm = TRUE))

  # check if all the samples keyed as MT1 hasn't lenghts
  false_mt1 <- merge(x = mt1, y = mt1_with_lenghts, by = BASE_FIELDS) %>%
    unique()
  
  if (nrow(false_mt1) > 0) {
    false_mt1 <- add_type_of_error(false_mt1, "ERROR: MT1 con tallas")
    return(false_mt1)
  }
  
  return(NULL)
}

#' Detect mixed species keyed as non mixed species
#' @details In COD_ESP_MUE there are codes from non mixed species
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
mixed_as_no_mixed <- function(catches) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")
  non_mixed <- merge(
    x = catches,
    y = especies_mezcla["COD_ESP_CAT"],
    by.x = "COD_ESP_MUE",
    by.y = "COD_ESP_CAT"
  )
  non_mixed <- non_mixed[, c(selected_fields)]
  
  if (nrow(non_mixed) > 0) {
    non_mixed <- add_type_of_error(
      non_mixed,
      "ERROR: especie de mezcla tecleada sin agrupar en Especies del Muestreo"
    )
    return(non_mixed)
  }
  
  return(NULL)
}

#' Detect no mixed species keyed as mixed species
#' @details In COD_ESP_MUE there are codes from mixed species
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1033
no_mixed_as_mixed <- function(lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT"
  )
  non_mixed <- merge(
    x = lengths,
    y = especies_no_mezcla["COD_ESP"],
    by.x = "COD_ESP_MUE",
    by.y = "COD_ESP"
  )

  
  if (nrow(non_mixed) > 0) {
    non_mixed <- non_mixed[, c(selected_fields)]
    non_mixed <- unique(non_mixed)

    non_mixed <- add_type_of_error(
      non_mixed,
      "ERROR: especie no de mezcla agrupada en Especies del Muestreo"
    )
    return(non_mixed)
  }
  
  return(NULL)
}

#' Detect grouped species in Species of the Category
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors
#' @note Check code: 1027
mixed_species_in_category <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT"
  )

  not_allowed_in_category <- especies_mezcla %>%
    select(COD_ESP_MUE) %>%
    unique()

  clean_catches_in_lenghts <- catches_in_lengths %>%
    select(any_of(selected_fields))

  errors <- merge(
    x = clean_catches_in_lenghts,
    y = not_allowed_in_category,
    by.x = "COD_ESP_CAT",
    by.y = "COD_ESP_MUE"
  )

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: muestreo MT2 con especie de mezcla que está agrupada en Especies para la Categoría"
    )
    return(errors)
  }
  
  return(NULL)
}

#' Detect doubtful species in Sampling Species
#' @param catches Data frame with catches data
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1029
doubtful_sampled_species <- function(catches) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")

  #create a dataframe with other species not allowed
  # by sufixex
  to_check_genus <- grep(
    "(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))",
    catches$ESP_MUE
  )
  # . = any single character
  # + = one of more of previous
  # | = or

  genus_not_allowed <- catches[to_check_genus, ]

  # remove the mixed species (allowed)
  errors <- genus_not_allowed[
    !(genus_not_allowed[["COD_ESP_MUE"]] %in%
      unique(mixed_species[["COD_ESP_MUE"]])),
  ] %>%
    select(any_of(selected_fields))

  # this is obsolete: when was allowed Loligo spp an Allotheuthis spp saved in
  # 'especies de la categoría':
  # remove other allowed species
  # genus_not_allowed <- genus_not_allowed[!(genus_not_allowed[["COD_ESP_MUE"]] %in% ALLOWED_GENUS[["COD_ESP"]]),] %>%
  #  select(any_of(selected_fields))
  

  if (nrow(errors) > 0) {
    errors <- unique(genus_not_allowed)
    errors <- add_type_of_error(
      errors,
      "WARNING: ¿seguro que es esa especie en Especies del Muestreo?"
    )
    return(errors)
  }
  
  return(NULL)
}


#' Detect if there are not allowed species in Sampling Species
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1028
not_allowed_sampled_species <- function(catches) {
  selected_fields <- c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")

  # create a dataframe with species not allowed
  sampling_species_not_allowed <- merge(
    x = catches,
    y = NOT_ALLOWED_SPECIES,
    by.x = "COD_ESP_MUE",
    by.y = "COD_ESP"
  ) %>%
    select(any_of(selected_fields))

  # remove duplicates
  errors <- unique(sampling_species_not_allowed)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: muestreo con especie no permitida en Especies del Muestreo"
    )
    return(errors)
  }
  
  return(NULL)
}

#' Detect sexes with exactly the same sampled weight
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1034
sexes_with_same_sampled_weight <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "P_MUE_DESEM"
  )

  errors <- catches_in_lengths %>%
    group_by_at(selected_fields) %>%
    summarise(NUM_SEXOS_MISMO_P_MUE_DESEM = n()) %>%
    filter(NUM_SEXOS_MISMO_P_MUE_DESEM > 1)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: Sexos de una misma especie tienen exactamente el mismo peso muestra"
    )
    return(errors)
  }
  
  return(NULL)
}


#' Detect categories with various equal sexes (both of them male or female)
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1035
categories_with_repeated_sexes <- function(catches_in_lengths) {
  selected_fields <- c(
    BASE_FIELDS,
    "COD_ESP_MUE",
    "ESP_MUE",
    "COD_CATEGORIA",
    "CATEGORIA",
    "COD_ESP_CAT",
    "ESP_CAT",
    "SEXO"
  )
  errors <- catches_in_lengths %>%
    filter(SEXO != "U") %>%
    group_by_at(selected_fields) %>%
    summarise(NUM_MISMOS_SEXOS = n()) %>%
    filter(NUM_MISMOS_SEXOS != 1)
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "categorías con varios sexos iguales (la misma especie con varias distribuciones de machos o hembras"
    )
    return(errors)
  }
  
  return(NULL)
}

#' Check if the COD_ID variable is filled
#' @details The COD_ID variable is filled when the data is dumped in SIRENO. But, if
#' a new sample is hand typed, the COD_ID it is not generated and is saved as
#' an empty field.
#' This function check that all the registers of COD_ID are filled
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return Return a dataframe with the erroneus codes
#' @note Check code: 1060
validate_cod_id <- function(lengths) {
  if (variable_exists_in_df("COD_ID", lengths)) {
    errors <- lengths[is.na(lengths[["COD_ID"]]), ]

    errors <- errors[, BASE_FIELDS]

    errors <- add_type_of_error(
      errors,
      "ERROR: variable COD_ID vacía. Este error solo puede ser resuelto por los servicios informáticos de Madrid."
    )

    return(errors)
  } else {
    return(FALSE)
  }
}


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


#' Detect if the content of variable TALL.PESO is allways TRUE
#' @details This logical variable is TRUE for lenghts samples and false to
#' weight samples.
#' We allways work with lengths samples so all of them must be TRUE
#' @param catches Data frame with catches data
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1046
validate_length_weight_variable <- function(catches) {
    errors <- catches %>%
      select(any_of(c(BASE_FIELDS, "TALL.PESO"))) %>%
      filter(TALL.PESO != "T" | is.na(TALL.PESO) | is.null(TALL.PESO)) %>%
      unique()
    
    if (nrow(errors) > 0) {
      errors <- add_type_of_error(errors, "ERROR: Muestreo no metido como muestreo de talla")
      return(errors)
    }
    
    return(NULL)
}


#' Detect samples with no sexed species that must be sexed
#' @details Sexed species must have the variable SEXO as M (male) or H (female)
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1047
sexed_species <- function(lengths) {
  errors <- lengths %>%
    select(any_of(c(
      BASE_FIELDS,
      "COD_ESP_MUE",
      "ESP_MUE",
      "COD_CATEGORIA",
      "CATEGORIA",
      "COD_ESP_CAT",
      "ESP_CAT",
      "SEXO"
    ))) %>%
    filter((COD_ESP_MUE %in% especies_sexadas[["COD_ESP"]])) %>% #only the sexed species
    merge(
      y = especies_sexadas,
      by.x = c("COD_ESP_CAT"),
      by.y = c("COD_ESP"),
      all.x = T
    ) %>% #merge with sexed species
    mutate(COD_PUERTO.y = as.character(COD_PUERTO.y)) %>% # <--- THIS IS IMPERATIVE BECAUSE
    #THE sexed_species DATAFRAME HAS DIFFERENT LEVELS THAN THE lengths DATAFRAME.
    filter(COD_PUERTO.y == "ALL" | COD_PUERTO.x == COD_PUERTO.y) %>% # sexed species
    #must be only with port field "ALL" or a port similiar between lengths and especies_sexadas dataframe
    filter((SEXO != "M" & SEXO != "H")) %>%
    rename(
      "COD_PUERTO" = COD_PUERTO.x,
      "PUERTO" = PUERTO.x,
      "LOCODE" = LOCODE.x,
      "ESP_MUE" = ESP_MUE.x
    )

  if (nrow(errors) > 0) {
    errors %>%
    select(any_of(c(
      BASE_FIELDS,
      "COD_ESP_MUE",
      "ESP_MUE",
      "COD_CATEGORIA",
      "CATEGORIA",
      "COD_ESP_CAT",
      "ESP_CAT",
      "SEXO"
    ))) %>%
    unique() %>%
    add_type_of_error("ERROR: especie que debería ser sexada")
    return(errors)
  }
  
  return(NULL)
}

#' Detect samples with sexed species that must not be sexed
#' @details No sexed species must have the variable SEXO as U
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1048
no_sexed_species <- function(lengths) {
  # Subset especies_sexadas dataframe only with required?? variables:
  sexed_species <- especies_sexadas[, c("COD_ESP", "COD_PUERTO")]

  # Subset sexed especies sampled
  sexed_species_sampled <- lengths %>%
    select(any_of(c(
      BASE_FIELDS,
      "COD_ESP_MUE",
      "ESP_MUE",
      "COD_CATEGORIA",
      "CATEGORIA",
      "COD_ESP_CAT",
      "ESP_CAT",
      "SEXO"
    ))) %>%
    filter(SEXO == "M" | SEXO == "H")

  # Check if all the species sampled must be sexed:
  errors_species_must_not_be_sexed <- sexed_species_sampled %>%
    filter(!(COD_ESP_CAT %in% sexed_species$COD_ESP)) %>%
    unique()

  # Check if all the species sampled must be sexed in some ports:
  sexed_species_by_port <- sexed_species[
    sexed_species[["COD_PUERTO"]] != "ALL",
  ]

  errors_species_must_be_sexed_only_in_some_ports <- sexed_species_sampled %>%
    filter(COD_ESP_CAT %in% sexed_species_by_port[["COD_ESP"]]) %>% # filter only species wich must be sampled in some ports
    merge(,
      y = sexed_species_by_port,
      by.x = c("COD_ESP_CAT", "COD_PUERTO"),
      by.y = c("COD_ESP", "COD_PUERTO"),
      all.x = T
    ) %>%
    unique()

  # merge errors
  errors <- rbind(
    errors_species_must_not_be_sexed,
    errors_species_must_be_sexed_only_in_some_ports
  )
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: especie que NO debería ser sexada. Es posible que el SOP de estos muestreos sea 0, por lo que se ha de comprobar."
    )
    return(errors)
  }
  
  return(NULL)
}


#' Detect samples with same date, vessel, gear, and port but with different
#' ESTRATO_RIM variable
#' @param catches Data frame with catches data
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1049
multiple_rim_stratum <- function(catches) {
  errors <- catches %>%
    select(any_of(BASE_FIELDS)) %>%
    unique() %>%
    group_by(
      COD_PUERTO,
      PUERTO,
      LOCODE,
      FECHA_MUE,
      COD_BARCO,
      BARCO,
      COD_TIPO_MUE,
      TIPO_MUE
    ) %>%
    mutate(num_estrato_rim = n_distinct(ESTRATO_RIM)) %>%
    ungroup() %>%
    # summarise(num_estrato_rim = n_distinct(ESTRATO_RIM)) %>%
    filter(num_estrato_rim != 1)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: mismo puerto/fecha/barco/tipo_muestreo con distinto ESTRATO_RIM"
    )
    return(errors)
  }
  
  return(NULL)
}

#' Detect multiple gear in the same trip: samples with same date, vessel,
#' ESTRATO_RIM, TIPO_MUE, and port but with different gear variable
#' @param catches Data frame with catches data
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1050
multiple_gear <- function(catches) {
  errors <- catches %>%
    select(any_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
    unique() %>%
    group_by(
      COD_PUERTO,
      PUERTO,
      LOCODE,
      FECHA_MUE,
      COD_BARCO,
      BARCO,
      ESTRATO_RIM,
      COD_TIPO_MUE,
      TIPO_MUE
    ) %>%
    mutate(num_arte = n_distinct(COD_ARTE)) %>%
    ungroup() %>%
    # summarise(num_Estrato_RIM = n_distinct(ESTRATO_RIM)) %>%
    filter(num_arte != 1)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: mismo puerto/fecha/barco/estrato_rim con distinto ARTE"
    )
    return(errors)
  }
  
  return(NULL)
}

#' Detecth multiple COD_PUERTO in the same trip: samples with same date,
#' vessel, ESTRATO_RIM, TIPO_MUE, and gear but with different port variable
#' @param catches Data frame with catches data
#' @return A data frame with erroneous samples if found, NULL otherwise
#' @note Check code: 1032
multiple_port <- function(catches) {
  # errors <- catches %>%
  #   select(any_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
  #   unique() %>%
  #   group_by(COD_ID, COD_PUERTO, PUERTO, LOCODE, FECHA_MUE, COD_BARCO, BARCO, ESTRATO_RIM, COD_TIPO_MUE, TIPO_MUE) %>%
  #   mutate(num_puerto = n_distinct(COD_PUERTO))%>%
  #   ungroup()%>%
  #   filter(num_puerto != 1) %>%
  #   add_type_of_error("ERROR: mismo fecha/barco/estrato_rim/tipo_muestre con distinto PUERTO")
  errors <- catches %>%
    select(any_of(c(BASE_FIELDS, "COD_ARTE", "ARTE"))) %>%
    unique() %>%
    group_by(FECHA_MUE, COD_BARCO, ESTRATO_RIM, COD_TIPO_MUE) %>%
    mutate(num_puerto = n()) %>%
    filter(num_puerto != 1)

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "ERROR: mismo fecha/barco/estrato_rim/tipo_muestreo con distinto PUERTO"
    )
    return(errors)
  }
  
  return(NULL)
}


#' Detect coherence between ESTRATO_RIM and origin
#' @param catches Data frame with catches data
#' @param specification Specification parameter
#' @return A data frame with wrong coherence
#' @note Check code: 1020
coherence_rim_stratum_origin <- function(catches, specification) {
  estrato_rim_origen$VALID <- TRUE

  df <- merge(
    x = catches,
    y = estrato_rim_origen,
    by.x = c("ESTRATO_RIM", "COD_ORIGEN"),
    by.y = c("ESTRATO_RIM", "COD_ORIGEN"),
    all.x = TRUE
  )

  errors <- df[is.na(df$VALID), ]

  if (nrow(errors) > 0) {
    errors <- errors[, c(BASE_FIELDS, "COD_ORIGEN")]
    errors <- unique(errors)
    errors <- add_type_of_error(
      errors,
      "ERROR: no concuerda el estrato_rim con el origen."
    )
    return(errors)
  }
  
  return(NULL)
}

#' Detect ESTRATO_RIM and gear of trips with 2 ships
#' @details Exception with Santa Eugenia de Ribeira port, that usually only one ship is
#' sampled
#' @param catches Data frame with catches data
#' @return A data frame with erroneous trips if found, NULL otherwise
#' @note Check code: 1022
ships_pair_bottom_trawl <- function(catches) {
  errors <- catches %>%
    select(any_of(BASE_FIELDS), N_BARCOS) %>%
    unique() %>%
    filter(COD_PUERTO != "0917") %>% #This is an execption: only one ship is sampled in Ribeira pair bottom trails
    filter(
      ((N_BARCOS == 2 | N_BARCOS == 3 | N_BARCOS == 4) &
        ESTRATO_RIM != "PAREJA_CN") |
        (N_BARCOS != 2 &
          N_BARCOS != 3 &
          N_BARCOS != 4 &
          ESTRATO_RIM == "PAREJA_CN")
    )
  
  if (nrow(errors) > 0) {
    errors <- add_type_of_error(errors, "ERROR: número de barcos no coherente con el estrato rim")
    return(errors)
  }
  
  return(NULL)
  #errors <- addInfluenceAreaVariable(errors, "COD_PUERTO")
  #write.csv(errors, file = "parejas_num_barcos_1.csv")
}

#' Detect lengths out of the size range of species in the rango_tallas_historico_caladero dataset
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with warnings lengths if found, NULL otherwise
#' @note Check code: 1052
lengths_outside_size_range <- function(lengths) {
  lengths <- merge(
    x = lengths,
    y = sapmuebase::caladero_origen[, c("CALADERO", "COD_ORIGEN")],
    all.x = TRUE,
    by = "COD_ORIGEN"
  )

  lengths <- merge(
    x = lengths,
    y = rango_tallas_historico_caladero,
    by.x = c("COD_ESP_CAT", "CALADERO"),
    by.y = c("COD_ESP", "CALADERO"),
    all.x = T
  )

  warningsOutOfRange <- lengths %>%
    select(
      any_of(BASE_FIELDS),
      COD_ESP_CAT,
      COD_CATEGORIA,
      TALLA,
      TALLA_MIN,
      TALLA_MAX
    ) %>%
    filter(TALLA < TALLA_MIN | TALLA > TALLA_MAX)
  
  if (nrow(warningsOutOfRange) > 0) {
    # it's not possible use add_type_of_error here, I don't know why
    warningsOutOfRange <- warningsOutOfRange %>%
      mutate(
        TIPO_ERROR = paste(
          "WARNING: Talla fuera del rango histórico de tallas por caladero:",
          TALLA_MIN,
          "-",
          TALLA_MAX
        )
      ) %>%
      humanizeVariable("COD_ESP_CAT")
    return(warningsOutOfRange)
  }
  
  return(NULL)
}
#' Detect species without historical range by fishing ground
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1065
species_without_historical_range <- function(lengths) {
  warningsIsRanged <- lengths %>%
    select(any_of(BASE_FIELDS), COD_ESP_CAT, COD_CATEGORIA) %>%
    merge(
      y = rango_tallas_historico_caladero,
      by.x = c("COD_ESP_CAT"),
      by.y = c("COD_ESP"),
      all.x = T
    ) %>%
    filter(is.na(TALLA_MIN) | is.na((TALLA_MAX))) %>%
    unique()
  
  if (nrow(warningsIsRanged) > 0) {
    warningsIsRanged <- warningsIsRanged %>%
      add_type_of_error(
        "WARNING: esta especie no se encuentra en el maestro histórico por caladero de tallas mínimas y máximas."
      ) %>%
      humanizeVariable("COD_ESP_CAT") %>%
      select(-c(TALLA_MIN, TALLA_MAX))
    return(warningsIsRanged)
  }
  
  return(NULL)
}


#' Detect outliers in species catches.
#' Uses the p99_capturas_historico dataset to detect if the total catch
#' of a species in an stratum exceed the 99th percentile of historical catches.
#' @param catches Data frame with catches data
#' @return A data frame with warnings if found, NULL otherwise
check_catches_p99 <- function(catches) {
  warnings <- catches %>%
    select(any_of(BASE_FIELDS), COD_ESP_MUE, ESP_MUE, P_DESEM) %>%
    group_by_at(c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE")) %>%
    summarise(P_DESEM_TOT = sum(P_DESEM)) %>%
    merge(
      .,
      p99_capturas_historico,
      by.x = c("ESTRATO_RIM", "COD_ESP_MUE"),
      by.y = c("ESTRATO_RIM", "COD_ESP"),
      all.x = T
    ) %>%
    filter(P_DESEM_TOT > P99)
  
  if (nrow(warnings) > 0) {
    warnings <- warnings %>%
      mutate(
        '% dif respecto al histórico de capturas' = round(
          ((P_DESEM_TOT - P99) * 100 / P_DESEM_TOT)
        )
      ) %>%
      add_type_of_error(
        "WARNING: Captura de la especie (de todas las categorías de la especie) superior al percentil 99 del histórico de capturas 2014 al 2018 por estrato rim."
      )

    warnings[['P99']] <- round(warnings[['P99']], 1)
    return(warnings)
  }
  
  return(NULL)
}

#' Detect incoherence between ESTRATEGIA and COD_TIPO_MUE
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
coherence_strategy_sample_type <- function(catches) {
  error_strategy <- catches %>%
    select(any_of(c(BASE_FIELDS, "ESTRATEGIA"))) %>%
    anti_join(y = tipo_muestreo, by = c("COD_TIPO_MUE", "ESTRATEGIA"))

  # MT2 samples of VORACERA_GC must be "En base a especie", so remove it of
  # the errors dataframe
  error_strategy <- error_strategy[
    -which(
      error_strategy$ESTRATO_RIM == "VORACERA_GC" &
        error_strategy$ESTRATEGIA == "En base a especie"
    ),
  ]

  if (nrow(error_strategy) > 0) {
    error_strategy <- add_type_of_error(
      error_strategy,
      "ERROR: No concuerda el campo ESTRATEGIA con el campo TIPO DE MUESTREO"
    )
    return(error_strategy)
  }
  
  return(NULL)
}

#' Detect trips with the same COD_BARCO and FECHA_MUE but different COD_TIPO_MUE
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
multiple_type_sample <- function(catches) {
  error <- catches %>%
    select(COD_ID, COD_PUERTO, COD_BARCO, FECHA_MUE, COD_TIPO_MUE) %>%
    unique() %>%
    group_by(COD_BARCO, FECHA_MUE) %>%
    mutate(number_type_sample = n_distinct(COD_TIPO_MUE, COD_BARCO)) %>%
    filter(number_type_sample > 1)

  if (nrow(error) > 0) {
    error <- error %>%
      add_type_of_error(
        "ERROR: Para un mismo barco y fecha, hay muestreos de varios tipos"
      )
    error <- humanize(error)
    return(error)
  }
  
  return(NULL)
}

#' Detect elapsed days between landing date and sampling date greater
#' than 3 or minor than 0
#' @param catches Data frame with catches data
#' @return A data frame with erroneous trips if found, NULL otherwise
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

#' Detect species which can have problems with taxonomic confusion
#' in both, catches and catches_in_lengths datasets.
#' The species which can have taxonomic confusion are stored
#' in the data set ESP_TAXONOMIC_CONFUSION.
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param catches_in_lengths Data frame with catches and lengths data from sapmuebase::importRIMCatchesInLengths()
#' @return A data frame with warnings if found, NULL otherwise
taxonomic_specie_confusion <- function(catches, catches_in_lengths) {
  err_catches <- catches %>%
    select(any_of(c(BASE_FIELDS, "COD_ORIGEN", "COD_ESP_MUE", "ESP_MUE"))) %>%
    unique %>%
    merge(,
      y = ESP_TAXONOMIC_CONFUSION,
      by.x = (c("COD_ESP_MUE", "COD_ORIGEN")),
      by.y = (c("COD_ESP_WARNING", "COD_ORIGEN"))
    ) %>%
    add_type_of_error(
      "WARNING: ¿seguro que la especie del muestreo no es la propuesta en ESP_PROPUESTA?"
    )

  err_catches_in_lengths <- catches_in_lengths %>%
    select(any_of(c(BASE_FIELDS, "COD_ORIGEN", "COD_ESP_CAT", "ESP_CAT"))) %>%
    unique %>%
    merge(,
      y = ESP_TAXONOMIC_CONFUSION,
      by.x = (c("COD_ESP_CAT", "COD_ORIGEN")),
      by.y = (c("COD_ESP_WARNING", "COD_ORIGEN"))
    ) %>%
    add_type_of_error(
      "WARNING: ¿seguro que la especie de la categoría no es la propuesta en ESP_PROPUESTA?"
    )

  err <- merge(err_catches, err_catches_in_lengths, all = TRUE) %>%
    select(-c(ESP_WARNING))

  colnames(err)[colnames(err) == "COMENTARIOS"] <- "COMENTARIO ESPECIE"

  if (nrow(err) > 0) {
    return(err)
  }
  
  return(NULL)
}


#' Search ships which landed in various ports at the same date.
#' There are a function 'checkMultiplePort' which find trips with FECHA_MUE,
#' COD_BARCO, ESTRATO_RIM, COD_TIPO_MUE. But this checkSameTripInVariousPorts
#' ignore ESTRATO_RIM and COD_TIPO_MUE variables.
#' @param catches Data frame with catches data
#' @return A data frame with warnings if found, NULL otherwise
same_trip_in_various_ports <- function(catches) {
  err <- catches %>%
    select(COD_PUERTO, FECHA_MUE, COD_BARCO) %>%
    unique() %>%
    group_by(FECHA_MUE, COD_BARCO) %>%
    mutate(n_ports_per_trip = n_distinct(COD_PUERTO)) %>%
    filter(n_ports_per_trip > 1) %>%
    sapmuebase::humanize()

  if (nrow(err) > 0) {
    err <- add_type_of_error(
      err,
      "ERROR: Un mismo barco ha descargado en varios puertos en la misma fecha. Es posible que el puerto pertenezca a un área de influencia distinta."
    )
    return(err)
  }
  
  return(NULL)
}


#' Check if the a variable is filled in all the rows
#' TODO: put in sapmuebase???
#' @param df Data frame to check
#' @param var Variable name to check
#' @return A data frame with errors
variable_not_filled <- function(df, var) {
  tryCatch(
    {
      variable_exists_in_df(var, df)

      fields <- c(BASE_FIELDS, var)

      err <- df[df[[var]] == "" | is.na(df[[var]]) | is.null(df[[var]]), fields]

      err <- unique(err)

      err <- add_type_of_error(err, "ERROR: campo ", substitute(var), " vacío.")

      return(err)
    },
    error = function(e) {
      print(e)
      return(NULL)
    }
  )
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

#' Detect if there are samples with the same name vessel but with different
#' SIRENO codification or SGPM codification
#' @param catches Data frame with catches data
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1061
multiple_ship_code <- function(catches) {
  # find the ship names with more than one COD_BARCO and COD_SGPM
  dsn <- catches %>%
    select(BARCO, COD_BARCO, COD_SGPM) %>%
    unique() %>%
    count(BARCO) %>%
    filter(n > 1)

  dsn <- as.character(dsn$BARCO)

  # get the samples with the multiple ships names
  err <- catches[catches$BARCO %in% dsn, ]

  err <- err[, BASE_FIELDS]
  err <- unique(err)
  
  if (nrow(err) > 0) {
    err <- add_type_of_error(
      err,
      "WARNING: Hay varios muestreos que tienen este mismo nombre de barco, pero con distinto código SIRENO o distinto Código Secretaría. ¿Seguro que es el barco correcto?"
    )
    return(err)
  }
  
  return(NULL)
}

#' Detect coherence between DCF Metier, Rim Stratum and Origin
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1062
coherence_dcf_metier_rim_stratum_origin <- function(catches) {
  metier_caladero_dcf_clean <- metier_caladero_dcf[, c(
    "ESTRATO_RIM",
    "METIER_DCF",
    "COD_ORIGEN"
  )]
  metier_caladero_dcf_clean$VALID <- TRUE

  catches_clean <- catches[, c(BASE_FIELDS, "COD_ORIGEN", "METIER_DCF")]
  catches_clean <- unique(catches_clean)
  catches_clean <- catches_clean[
    !(is.na(catches_clean$COD_ORIGEN) | catches_clean$COD_ORIGEN == ""),
  ]
  catches_clean <- catches_clean[
    !(is.na(catches_clean$METIER_DCF) | catches_clean$METIER_DCF == ""),
  ]

  err <- merge(
    catches_clean,
    metier_caladero_dcf_clean,
    by = c("ESTRATO_RIM", "METIER_DCF", "COD_ORIGEN"),
    all.x = T
  )

  err <- err[which(err$VALID != TRUE | is.na(err$VALID)), ]

  if (nrow(err) > 0) {
    err <- add_type_of_error(
      err,
      "El Metier DCF no concuerda con el estrato RIM y el origen"
    )
    return(err)
  }
  
  return(NULL)
}

#' Detect coherence between DCF Fishing Ground, Rim Stratum and Origin
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1063
coherence_dcf_fishing_ground_rim_stratum_origin <- function(catches) {
  metier_caladero_dcf_clean <- metier_caladero_dcf[, c(
    "ESTRATO_RIM",
    "CALADERO_DCF",
    "COD_ORIGEN"
  )]
  metier_caladero_dcf_clean$VALID <- TRUE

  catches_clean <- catches[, c(BASE_FIELDS, "COD_ORIGEN", "CALADERO_DCF")]
  catches_clean <- unique(catches_clean)
  # catches_clean$CALADERO_DCF <- as.character(catches_clean$CALADERO_DCF)
  catches_clean <- catches_clean[
    !(is.na(catches_clean$COD_ORIGEN) | catches_clean$COD_ORIGEN == ""),
  ]
  catches_clean <- catches_clean[
    !(is.na(catches_clean$CALADERO_DCF) | catches_clean$CALADERO_DCF == ""),
  ]

  err <- merge(
    catches_clean,
    metier_caladero_dcf_clean,
    by = c("ESTRATO_RIM", "CALADERO_DCF", "COD_ORIGEN"),
    all.x = T
  )

  err <- err[which(err$VALID != TRUE | is.na(err$VALID)), ]

  if (nrow(err) > 0) {
    err <- add_type_of_error(
      err,
      "El Caladero DCF no concuerda con el estrato RIM y el origen"
    )
    return(err)
  }
  
  return(NULL)
}

#' Detect all the categories are measured. If one or more are not, return a
#' warning
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1064
all_categories_measured <- function(catches, lengths) {
  # Some categories never are measured, so must be ignored
  regex_to_ignore <- c(
    "^.*(?i)colas.*",
    "^.*(?i)melad(a|o)$",
    "^.*(?i)melad(a|o).*",
    "^.*(?i)trozos$",
    "^.*(?i)picad(a|o)",
    "^(?i)alas\ .*$",
    "^Huevas$"
  )

  regex_to_ignore <- paste(regex_to_ignore, collapse = "|")

  # Clean the categories master
  maestro_categorias_clean <- categorias[, c(
    "COD_ESP",
    "COD_PUERTO",
    "COD_CATEGORIA",
    "PROCESO"
  )]

  # Clean catches dataframe
  clean_catches <- catches[which(catches$COD_TIPO_MUE == "2"), ]

  clean_catches <- merge(
    clean_catches,
    maestro_categorias_clean,
    by.x = c("COD_ESP_MUE", "COD_PUERTO", "COD_CATEGORIA"),
    by.y = c("COD_ESP", "COD_PUERTO", "COD_CATEGORIA"),
    all.x = T
  )

  clean_catches <- clean_catches[
    -grep(regex_to_ignore, clean_catches$CATEGORIA),
  ]

  # Get the number of categories in catches dataframe
  cat_cat <- clean_catches %>%
    group_by(COD_ID, COD_ESP_MUE, ESP_MUE) %>%
    mutate(n_cat_catches = n_distinct(COD_CATEGORIA)) %>%
    select(all_of(BASE_FIELDS), COD_ESP_MUE, ESP_MUE, n_cat_catches) %>%
    unique()

  # Get the number of categories with sampled lengths
  sam_cat_len <- lengths %>%
    group_by(COD_ID, COD_ESP_MUE, ESP_MUE) %>%
    mutate(n_cat_lengths = n_distinct(COD_CATEGORIA)) %>%
    select(COD_ID, COD_ESP_MUE, ESP_MUE, n_cat_lengths) %>%
    unique()

  # Get errors

  errors <- merge(
    cat_cat,
    sam_cat_len,
    all.x = T,
    by = c("COD_ID", "COD_ESP_MUE", "ESP_MUE")
  )

  errors <- errors[
    which(
      errors[["n_cat_catches"]] > 1 &
        errors[["n_cat_lengths"]] < errors[["n_cat_catches"]]
    ),
  ]

  if (nrow(errors) > 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: algunas categorías de la especie están muestreadas pero otras no"
    )

    errors <- errors[, c(BASE_FIELDS, "COD_ESP_MUE", "ESP_MUE", "TIPO_ERROR")]

    return(errors)
  }
  
  return(NULL)
}


#' Detect if the value of variable is in prescriptions dataset
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


#' Detect if the variables "COD_PUERTO", "COD_ARTE", "COD_ORIGEN", "ESTRATO_RIM",
#' "METIER_DCF" and "CALADERO_DCF" are coherent with MT2 rim prescriptions
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return A data frame with errors if found, NULL otherwise
#' @note Check code: 1069
coherence_rim_mt2_prescriptions_post <- function(catches) {
  catches <- catches[catches[["COD_TIPO_MUE"]] == 2, ] #THIS IS DIFFERENT IN rim_pre_dump, COD_TIPO_MUE is "MT2A"

  errors <- unique(catches[, c(
    "COD_ID",
    "FECHA_MUE",
    "COD_BARCO",
    "BARCO",
    "COD_PUERTO",
    "COD_ARTE",
    "COD_ORIGEN",
    "ESTRATO_RIM",
    "METIER_DCF",
    "CALADERO_DCF"
  )])
  errors <- merge(
    errors,
    prescripciones_rim_mt2_coherencia,
    by = c(
      "COD_PUERTO",
      "COD_ARTE",
      "COD_ORIGEN",
      "ESTRATO_RIM",
      "METIER_DCF",
      "CALADERO_DCF"
    ),
    all.x = TRUE
  )
  if (nrow(errors) > 0) {
    errors <- errors[
      is.na(errors[["PESQUERIA"]]),
      c(
        "COD_ID",
        "FECHA_MUE",
        "COD_BARCO",
        "BARCO",
        "COD_PUERTO",
        "COD_ARTE",
        "COD_ORIGEN",
        "METIER_DCF",
        "CALADERO_DCF"
      )
    ]
    
    if (nrow(errors) > 0) {
      errors <- humanize(errors)
      errors <- add_type_of_error(
        errors,
        "Esta combinación de puerto, arte, origen, estrato rim, metier DCF y caladero DCF no está en las prescripciones MT2 RIM de 2021."
      )
      return(errors)
    }
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

#' Detect G1 priority species with catches but not measured
#' @details Use MT2A and MT2B samples
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return Data frame with species of the priority group without samples measured
#' @note Check code: 1077
g1_species_not_measured <- function(catches, lengths) {
  errors <- priority_species_not_measured(catches, lengths, "G1")

  if (nrow(errors) != 0) {
    errors <- add_type_of_error(errors, "ERROR: especie prioritaria G1 no muestreada.")
    return(errors)
  }
  
  return(NULL)
}

#' Detect G2 priority species with catches but not measured
#' @details Use MT2A and MT2B samples
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return Data frame with species of the priority group without samples measured
#' @note Check code: 1078
g2_species_not_measured <- function(catches, lengths) {
  errors <- priority_species_not_measured(catches, lengths, "G2")

  if (nrow(errors) != 0) {
    errors <- add_type_of_error(
      errors,
      "WARNING: especie prioritaria G2 no muestreada."
    )
    return(errors)
  }
  
  return(NULL)
}


#' Function to check the presence of the ships from our ships' masterdata on the
#' working data
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param shipFishingGearMaster master dataframe with the code ships and its
#' ESTRATO_RIM from 2020 to 2022
#' @return Data frame with those ships no presents in our working data if found, NULL otherwise
#' @note Check code: 1079 PENDIENTE DE IMPLEMENTAR --> FALTA CREAR MAESTRO
ship_not_in_master_fishing_gear <- function(catches, shipFishingGearMaster) {
  #Step 1: filter the necessary columns from the catches df and pass from level to normal data

  catches <- catches[, BASE_FIELDS]

  catches$COD_BARCO <- as.character(catches$COD_BARCO)

  catches$ESTRATO_RIM <- as.character(catches$ESTRATO_RIM)

  #Step 2: filter the ships in the catches' dataframe that are not present in boatFishingArtMaster

  masterShip <- unique(shipFishingGearMaster$COD_BARCO)

  catchesNoMasterShip <- catches[!(catches$COD_BARCO %in% masterShip), ]

  #Step 3: check that the number of road is not equal to zero and add the type or error for obtained dataframes

  if (nrow(catchesNoMasterShip) != 0) {
    catchesNoMasterShip <- add_type_of_error(
      catchesNoMasterShip,
      "WARNING: Barco no presente en el maestro"
    )
    
    catchesNoMasterShip <- unique(catchesNoMasterShip)
    
    return(catchesNoMasterShip)
  }
  
  return(NULL)
}

#' Function to check the coherence of the ESTRATO_RIM between our ships' masterdata
#' and the working data
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @param shipFishingGearMaster master dataframe with the code ships and its
#' ESTRATO_RIM from 2020 to 2022
#' @return Data frame that mark those ships where there is not coincidence if found, NULL otherwise
#' @note Check code: 1080 NOT IMPLEMENTED --> FALTA CREAR EL MAESTRO
ship_different_fishing_gear <- function(catches, shipFishingGearMaster) {
  #Step 1: add the "Testigo" column to shipFishingGearMaster and convert "COD_BARCO" and "ESTRATO_RIM" as parameter

  shipFishingGearMaster$TESTIGO <- "T"

  shipFishingGearMaster$COD_BARCO <- as.character(
    shipFishingGearMaster$COD_BARCO
  )

  shipFishingGearMaster$ESTRATO_RIM <- as.character(
    shipFishingGearMaster$ESTRATO_RIM
  )

  #Step 2: filter the necessary columns from the catches df and pass from level to normal data

  catches <- catches[, BASE_FIELDS]

  catches$COD_BARCO <- as.character(catches$COD_BARCO)

  catches$ESTRATO_RIM <- as.character(catches$ESTRATO_RIM)

  #Step 3: filter the ships in the catches' dataframe that are present in shipFishingGearMaster

  masterShip <- unique(shipFishingGearMaster$COD_BARCO)

  catchesMasterShip <- catches[catches$COD_BARCO %in% masterShip, ]

  #Step 4: merging the shipFishingGearMaster frame with the catches' one

  catchesMasterShip <- merge(
    catchesMasterShip,
    shipFishingGearMaster,
    all.x = TRUE
  )

  #Step 5: develop new dataframes, one to detect where the TESTIGO values are "Na"

  catchesNaValues <- unique(catchesMasterShip[
    is.na(catchesMasterShip$TESTIGO),
  ])

  #Step 6: now, we make a subset of the masterShip with the ship's codes of the catchesNaValues.
  #Then, we create a special dataframe where we have unite all ESTRATOS that has one ship
  #using the function «concatenaThor».

  shipFishingGearMasterNaValues <- shipFishingGearMaster[
    shipFishingGearMaster$COD_BARCO %in% catchesNaValues$COD_BARCO,
    c("COD_BARCO", "ESTRATO_RIM")
  ]

  fusionEstratos <- tapply(
    shipFishingGearMasterNaValues$ESTRATO_RIM,
    shipFishingGearMasterNaValues$COD_BARCO,
    paste,
    collapse = ", "
  )

  shipCodes <- unique(shipFishingGearMasterNaValues$COD_BARCO)

  fusionEstratosDataFrame <- data.frame(
    COD_BARCO = shipCodes,
    ESTRATOS = fusionEstratos
  )

  #Step 7: here we merge the fusionEstratosDataFrame with the catchesNaValues

  catchesNaValues <- merge(
    catchesNaValues,
    fusionEstratosDataFrame,
    all.x = TRUE
  )

  #Step 8: check that the number of road is not equal to zero and add the type or error for obtained dataframes

  if (nrow(catchesNaValues) != 0) {
    message <- paste0(
      "WARNING: El barco trabajó en 2020-2022 en otros estratos diferentes: ",
      catchesNaValues$ESTRATOS
    )
    catchesNaValues$TIPO_ERROR <- message
    
    #Note: making a little fix in the columns of the catchesNaValues

    catchesNaValues <- catchesNaValues[, c(BASE_FIELDS, "TIPO_ERROR")]

    catchesNaValues <- unique(catchesNaValues)

    return(catchesNaValues)
  }
  
  return(NULL)
}


#' Detect ships with "COD_SGPM" value empty, "0" or NA
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return Data frame with errors if found, NULL otherwise
#' @note Check code: 1081
ship_without_cod_sgpm <- function(catches) {
  catches <- catches[
    catches$COD_SGPM == "0" | catches$COD_SGPM == "" | is.na(catches$COD_SGPM),
    c(BASE_FIELDS, "COD_SGPM")
  ]

  if (nrow(catches) != 0) {
    catches <- unique(catches)
    catches <- add_type_of_error(
      catches,
      "ERROR: El código de la SGPM para este barco está vacio."
    )
    return(catches)
  }
  
  return(NULL)
}

#' Detect categories with code ended in '99' and its sample type is different of
#' "MT2B(Muestreo a bordo)" or "MT3 (Muestreo dirigido)"
#' @details Categories ended in '99' can only be assigned to one of those sample types
#' @param catches Data frame with catches data from sapmuebase::importRIMCatches()
#' @return Data frame with errors if found, NULL otherwise
#' @note Check code: 1082
categories_99_not_in_mt2b <- function(catches) {
  catches <- catches[
    !(catches$COD_TIPO_MUE %in% c("4", "6")) &
      grepl("99$", catches$COD_CATEGORIA),
    BASE_FIELDS
  ]

  if (nrow(catches) != 0) {
    catches <- add_type_of_error(
      catches,
      "ERROR: el código COD_CATEGORIA acaba en 99 pero el tipo de muestreo NO es MT2B(Muestreo a bordo). Las categorías que acaban en 99 están reservadas sólo para los tipo de muestreo MT2B(Muestreo a bordo) o MT3 (Muestreo dirigido)"
    )

    return(catches)
  }
  
  return(NULL)
}

# TODO: This function detects the checked samples, not the validated ones
#' Detect samples that are not validated
#' @details Column "VALIDADO" from length dataframe with FALSE or NA values
#' @param lengths Data frame with lengths data from sapmuebase::importRIMLengths()
#' @return Data frame with errors if found, NULL otherwise
#' @note Check code: 1083
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



