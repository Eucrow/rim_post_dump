rim_check_annual <- function(samples_imported) {
  tryCatch({
    # filter by sample type
    sample_types <- c(1, 2, 6)

    samples_imported <- lapply(samples_imported, function(x) {
      out <- x[x[["COD_TIPO_MUE"]] %in% sample_types, ]
      return(out)
    })

    catches <- samples_imported$catches
    catches_in_lengths <- samples_imported$catches_in_lengths
    lengths_sampled <- samples_imported$lengths

    err <- list()

    # ---- REPEATED IN IPDTOSIRENO ----

    err$estrato_rim <- check_variable_with_master(catches, "ESTRATO_RIM")

    err$puerto <- check_variable_with_master(catches, "COD_PUERTO")

    err$arte <- check_variable_with_master(catches, "COD_ARTE")

    err$origen <- check_variable_with_master(catches, "COD_ORIGEN")

    err$procedencia <- check_variable_with_master(catches, "PROCEDENCIA")

    err$tipo_muestreo <- check_variable_with_master(catches, "COD_TIPO_MUE")

    err$false_MT1 <- check_false_mt1(catches, lengths_sampled)

    err$false_MT2 <- check_false_mt2(catches, lengths_sampled)

    err$no_mixed_as_mixed <- check_no_mixed_as_mixed(lengths_sampled)

    err$mixed_as_no_mixed <- check_mixed_as_no_mixed(catches)

    # err$estrato_rim_prescriptions <- check_variable_with_rim_mt2_prescriptions_post(catches, "ESTRATO_RIM")
    # err$puerto_prescriptions <- check_variable_with_rim_mt2_prescriptions_post(catches, "COD_PUERTO")
    # err$origen_prescriptions <- check_variable_with_rim_mt2_prescriptions_post(catches, "COD_ORIGEN")
    # err$arte_prescriptions <- check_variable_with_rim_mt2_prescriptions_post(catches, "COD_ARTE")
    # err$metier_dcf_prescriptions <- check_variable_with_rim_mt2_prescriptions_post(catches, "METIER_DCF")
    # err$caladero_dcf_prescriptions <- check_variable_with_rim_mt2_prescriptions_post(catches, "CALADERO_DCF")

    err$empty_fields_in_variables_catches <- empty_fields_in_variables(
      catches,
      "RIM_CATCHES"
    )
    err$empty_fields_in_variables_lengths <- empty_fields_in_variables(
      lengths_sampled,
      "RIM_LENGTHS"
    )

    # ---- IN HEADER ----

    err$errors_mt2b_rim_stratum <- check_mt2b_rim_stratum(catches)

    err$coherence_estrato_rim_gear <- coherence_rim_stratum_gear(catches, "RIM")

    err$coherence_estrato_rim_origin <- check_coherence_rim_stratum_origin(
      catches,
      "RIM"
    )

    # err$coherence_rim_mt2_prescriptions <- coherence_rim_mt2_prescriptions_post(catches)

    err$no_ships <- check_no_ships(catches)
    
    err$more_than_two_ships <- check_more_than_two_ships(catches)

    err$number_of_rejections <- number_of_rejections(catches)

    err$errors_countries_mt1 <- check_foreing_ships_MT1(catches)

    err$errors_countries_mt2 <- check_foreing_ships_MT2(catches)

    err$multiple_ship_code <- check_multiple_ship_code(catches)

    ##### TO DO: ADD CHECKING SHIPS WITH SIRENO FILES

    err$errors_ships_not_in_cfpo <- ships_not_in_cfpo(catches)

    # no_en_cfpo <- err$errors_ships_not_in_cfpo %>%
    #   filter(!grepl("^8\\d{5}",COD_BARCO) & COD_BARCO != 0) %>%
    #   select(COD_BARCO, BARCO, COD_PUERTO, PUERTO)%>%
    #   unique()

    err$errors_ships_not_registered <- ships_not_registered(catches)

    err$errors_multiple_estrato_rim <- check_multiple_rim_stratum(catches)

    err$errors_multiple_arte <- check_multiple_gear(catches)

    err$errors_multiple_puerto <- check_port_mismatch_for_same_trip(catches)

    err$errors_num_barcos_pareja <- check_ships_pair_bottom_trawl(catches)

    err$estrategia <- check_strategy(catches)

    err$multiple_tipo_muestreo <- multiple_type_sample(catches)

    err$tiempo_transcurrido <- check_elapsed_days(catches)

    err$check_same_trip_in_various_ports <- check_same_trip_in_various_ports(catches)

    err$checkSampleInCharge <- check_variable_filled(
      catches,
      "RESPONSABLE_MUESTREO"
    )

    err$ship_without_cod_sgpm <- ship_without_cod_sgpm(catches)

    # This errors must be used only in anual, when the Fishing Ground and DCF
    # Metier is filled:
    # err$coherence_dcf_fishing_ground_rim_stratum_origin <- coherence_dcf_fishing_ground_rim_stratum_origin(catches)
    # err$coherence_dcf_metier_rim_stratum_origin <- coherence_dcf_metier_rim_stratum_origin(catches)

    # ---- IN SPECIES ----

    err$mixed_species_category <- mixed_species_in_category(catches_in_lengths)

    err$not_allowed_sampled_species <- not_allowed_sampled_species(catches)

    err$sampled_species_doubtful <- doubtful_sampled_species(catches)

    err$not_allowed_category_species <- not_allowed_category_species(
      catches_in_lengths
    )

    err$doubtful_category_species <- doubtful_category_species(catches_in_lengths)

    err$sexes_with_same_sampled_weight <- sexes_with_same_sampled_weight(
      catches_in_lengths
    )

    err$categories_with_repeated_sexes <- categories_with_repeated_sexes(
      catches_in_lengths
    )

    err$lenghts_weights_sample <- check_length_weight_variable(catches)

    err$no_sexed_species <- check_no_sexed_species(lengths_sampled)

    err$sexed_species <- check_sexed_species(lengths_sampled)

    err$taxonomic_specie_confusion <- taxonomic_specie_confusion(
      catches,
      catches_in_lengths
    )
    
    err$new_species_sampled <- check_new_species_sampled(catches_in_lengths)

    # TODO: FIND A BETTER WAY TO CHECK THIS, WHICH ADD THE SPECIES NAME
    # err$a3CodeFilled <- check_variable_filled(catches, "A3_ESP_MUE")

    # ---- IN WEIGHTS ----

    err$same_sampled_weight <- all_categories_with_same_sampled_weights(
      catches_in_lengths
    )

    err$sampled_weight_zero <- weight_sampled_zero_with_lengths_sampled(
      catches_in_lengths
    )

    err$weight_landed_zero <- weight_landed_zero(catches)

    err$weight_sampled_without_length_sampled <- weight_sampled_without_lengths_sampled(
      catches_in_lengths
    )

    err$pes_mue_desem_zero <- pes_mue_desem_zero(catches_in_lengths)

    err$especies_con_categorias_igual_peso_desembarcado <- species_with_categories_with_same_weight_landing(
      catches
    )

    err$sop_zero <- sop_zero(catches_in_lengths)

    err$sop_greater_pes_mue_vivo <- sop_greater_pes_mue_vivo(catches_in_lengths)

    err$sop_mayor_peso_vivo <- sop_greater_pes_vivo(catches_in_lengths)

    err$pes_mue_desem_mayor_pes_desem <- pes_mue_desem_greater_pes_desem(
      catches_in_lengths
    )

    # TODO: FIND A BETTER WAY TO CHECK THIS, WHICH ADD THE SPECIES NAME
    # err$a3CodeFilled <- check_variable_filled(catches_in_lengths, "A3_ESP_CAT")

    err$categories_99_not_in_mt2b <- categories_99_not_in_mt2b(catches)

    # comment in annual:
    # err$capturas_percentil_99 <- check_catches_p99(catches)

    # ---- IN LENGTHS ----
    err$all_categories_measured <- all_categories_measured(
      catches,
      lengths_sampled
    )

    err$check_cm_measures <- check_cm_measures(lengths_sampled)

    err$check_half_cm_measures <- check_half_cm_measures(lengths_sampled)

    # comment in annual:
    # err$with_historical_size_range <- check_range_in_historical(muestreos_up$lengths)

    # comment in annual:
    # err$size_range <- check_size_range_by_fishing_ground(muestreos_up$lengths)
    # err$g1_species_not_measured <- g1_species_not_measured(catches, lengths_sampled)
    # err$g2_species_not_measured <- g2_species_not_measured(catches, lengths_sampled)

    # ---- COD_ID ----
    # This check is useful in the annual review. When the data is dumped in
    # SIRENO, COD_ID is automatically filled. But, if later someone add a new
    # sample, the COD_ID doesn't fill and is saved as empty.
    err$cod_id_filled_catches <- check_cod_id(catches)
    err$cod_id_filled_catches_in_lengths <- check_cod_id(catches_in_lengths)
    err$cod_id_filled_lengths <- check_cod_id(lengths_sampled)
    
    # ---- CHEQUEADO ----
    # This check is useful in the annual review if any trip was
    # not checked during the year
    
    err$unchecked_trip <- trip_is_checked(catches)

    # ---- COMBINE ERRORS ----

    # separated by influence area
    combined_errors <- format_errors_list(errors_list = err, separate_by_ia = T)

    return(combined_errors)
  })
}
