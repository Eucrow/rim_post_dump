oab_check <- function (samples_imported) {

  err <- list()

  # this check must be done before filter by sample type:
  err$multiple_tipo_muestreo <- multiple_type_sample(samples_imported$catches)


  tryCatch({

    #filter by sample type
    sample_types <- c(4)

    samples_imported <- lapply(samples_imported, function(x){
      out <- x[x[["COD_TIPO_MUE"]] %in% sample_types, ]
      return(out)
    })

    catches <- samples_imported$catches
    catches_in_lengths <- samples_imported$catches_in_lengths
    lengths_sampled <- samples_imported$lengths



    # ---- REPEATED IN IPDTOSIRENO ----

    err$estrato_rim <- check_variable_with_master(catches, "ESTRATO_RIM")

    # err$puerto <- check_variable_with_master(catches, "COD_PUERTO")

    err$arte <- check_variable_with_master(catches, "COD_ARTE")

    err$origen <- check_variable_with_master(catches, "COD_ORIGEN")

    err$procedencia <- check_variable_with_master(catches, "PROCEDENCIA")

    # err$tipo_muestreo <- check_variable_with_master(catches, "COD_TIPO_MUE")

    err$false_MT1 <- detect_false_mt1(catches, lengths_sampled)

    err$false_MT2 <- detect_false_mt2(catches, lengths_sampled)

    err$no_mixed_as_mixed <- no_mixed_as_mixed(lengths_sampled)

    err$mixed_as_no_mixed <- mixed_as_no_mixed(catches)

    # this is new... I don't know if can be applied in oab in rim samples:
    # TODO: test it.
    err$estrato_rim_prescriptions <- variable_with_rim_mt2_prescriptions_post(catches, "ESTRATO_RIM")
    err$puerto_prescriptions <- variable_with_rim_mt2_prescriptions_post(catches, "COD_PUERTO")
    err$origen_prescriptions <- variable_with_rim_mt2_prescriptions_post(catches, "COD_ORIGEN")
    err$arte_prescriptions <- variable_with_rim_mt2_prescriptions_post(catches, "COD_ARTE")
    err$metier_dcf_prescriptions <- variable_with_rim_mt2_prescriptions_post(catches, "METIER_DCF")
    err$caladero_dcf_prescriptions <- variable_with_rim_mt2_prescriptions_post(catches, "CALADERO_DCF")

    err$empty_fields_in_variables_catches <- empty_fields_in_variables(catches, "RIM_CATCHES")
    err$empty_fields_in_variables_lengths <- empty_fields_in_variables(lengths_sampled, "RIM_LENGTHS")


    # ---- IN HEADER ----

    # err$errors_mt2b_rim_stratum <- validate_mt2b_rim_stratum(catches)

    err$coherence_estrato_rim_gear <- coherence_rim_stratum_gear(catches)

    err$coherence_estrato_rim_origin <- coherence_rim_stratum_origin(catches)

    # err$coherence_rim_mt2_prescriptions <- coherence_rim_mt2_prescriptions_post(catches)

    err$number_of_ships <- number_of_ships(catches)

    # err$number_of_rejections <- number_of_rejections(catches)

    err$errors_countries_mt1 <- foreign_ships_mt1(catches)

    err$errors_countries_mt2 <- foreign_ships_mt2(catches)

    err$multiple_ship_code <- multiple_ship_code(catches)


    ##### TO DO: ADD CHECKING SHIPS WITH SIRENO FILES

    err$errors_ships_not_in_cfpo <-ships_not_in_cfpo(catches)

    # no_en_cfpo <- err$errors_ships_not_in_cfpo %>%
    #   filter(!grepl("^8\\d{5}",COD_BARCO) & COD_BARCO != 0) %>%
    #   select(COD_BARCO, BARCO, COD_PUERTO, PUERTO)%>%
    #   unique()

    err$errors_ships_not_registered <- ships_not_registered(catches)

    err$errors_multiple_estrato_rim <- multiple_rim_stratum(catches)

    err$errors_multiple_arte <- multiple_gear(catches)

    err$errors_multiple_puerto <- multiple_port(catches)

    err$errors_num_barcos_pareja <- ships_pair_bottom_trawl(catches)

    err$estrategia <- check_strategy(catches)

    # this is done before filter by type sample:
    # err$multiple_tipo_muestreo <- multiple_type_sample(catches)

    err$tiempo_transcurrido <- check_elapsed_days(catches)

    err$check_same_trip_in_various_ports <- check_same_trip_in_various_ports(catches)

    err$checkSample_In_Charge <- check_variable_filled(catches, "RESPONSABLE_MUESTREO")

    # This errors must be used only in anual, when the Fishing Ground and DCF
    # Metier is filled:

    # err$coherence_dcf_fishing_ground_rim_stratum_origin <- coherence_dcf_fishing_ground_rim_stratum_origin(catches)
    #
    # err$coherence_dcf_metier_rim_stratum_origin <- coherence_dcf_metier_rim_stratum_origin(catches)

    # ---- IN SPECIES ----

    err$mixed_species_category <- mixed_species_in_category(catches_in_lengths)

    err$not_allowed_sampled_species <- not_allowed_sampled_species(catches)

    err$sampled_species_doubtful <- doubtful_sampled_species(catches)

    err$not_allowed_category_species <- not_allowed_category_species(catches_in_lengths)

    err$doubtful_category_species <- doubtful_category_species(catches_in_lengths)

    err$sexes_with_same_sampled_weight <- sexes_with_same_sampled_weight(catches_in_lengths)

    err$categories_with_repeated_sexes <- categories_with_repeated_sexes(catches_in_lengths)

    err$lenghts_weights_sample <- validate_length_weight_variable(catches)

    err$no_sexed_species <- no_sexed_species(lengths_sampled)

    err$sexed_species <- sexed_species(lengths_sampled)

    err$taxonomic_specie_confusion <- taxonomic_specie_confusion(catches, catches_in_lengths)

    # TODO: FIND A BETTER WAY TO CHECK THIS, WHICH ADD THE SPECIES NAME
    # err$a3CodeFilled <- check_variable_filled(catches, "A3_ESP_MUE")


    # ---- IN WEIGHTS ----

    err$same_sampled_weight <- all_categories_with_same_sampled_weights(catches_in_lengths)

    err$sampled_weight_zero <- weight_sampled_zero_with_lengths_sampled(catches_in_lengths)

    err$weight_landed_zero <- weight_landed_zero(catches)

    err$weight_sampled_without_length_sampled <- weight_sampled_without_lengths_sampled(catches_in_lengths)

    err$pes_mue_desem_zero <- pes_mue_desem_zero(catches_in_lengths)

    err$especies_con_categorias_igual_peso_desembarcado <- species_with_categories_with_same_weight_landing(catches)

    err$sop_zero <- sop_zero(catches_in_lengths)

    err$sop_greater_pes_mue_vivo <- sop_greater_pes_mue_vivo(catches_in_lengths)

    err$sop_mayor_peso_vivo <- sop_greater_pes_vivo(catches_in_lengths)

    err$pes_mue_desem_mayor_pes_desem <- pes_mue_desem_greater_pes_desem(catches_in_lengths)

    # TODO: FIND A BETTER WAY TO CHECK THIS, WHICH ADD THE SPECIES NAME
    # err$a3CodeFilled <- check_variable_filled(catches_in_lengths, "A3_ESP_CAT")

    # comment in annual:
    # err$capturas_percentil_99 <- check_catches_p99(catches)


    # ---- IN LENGTHS ----
    err$with_historical_size_range <- species_without_historical_range(lengths_sampled)
    err$size_range <- lengths_outside_size_range(lengths_sampled)


    # ---- COD_ID ----
    # This check is usefull in the anual review. When the data is dumped in
    # SIRENO, COD_ID is automatically filled. But, if later someone add a new
    # sample, the COD_ID doesn't fill and is saved as empty.
    err$cod_id_filled_catches <- validate_cod_id(catches)
    err$cod_id_filled_catches_in_lengths <- validate_cod_id(catches_in_lengths)
    err$cod_id_filled_lengths <- validate_cod_id(lengths_sampled)

    # ---- COMBINE ERRORS ----

    #separated by influence area
    combined_errors <- format_errors_list(errors_list = err, separate_by_ia = F)

    # ---- ADD COMMENTS VARIABLE (in comments is where the original trip code of
    # the trip are stored.)
    combined_errors[["total"]] <- merge(combined_errors[["total"]],
                             unique(catches[,c("COD_ID", "OBSERVACIONES")]),
                             by.x="COD_ID",
                             by.y="COD_ID",
                             all.x=TRUE)
    return(combined_errors)

  })
}
