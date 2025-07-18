rim_check_annual_nvdp_matched <- function(samples_imported) {
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

    # err$estrato_rim <- check_variable_with_master(catches, "ESTRATO_RIM")
    #
    # err$puerto <- check_variable_with_master(catches, "COD_PUERTO")
    #
    # err$arte <- check_variable_with_master(catches, "COD_ARTE")
    #
    # err$origen <- check_variable_with_master(catches, "COD_ORIGEN")
    #
    # err$procedencia <- check_variable_with_master(catches, "PROCEDENCIA")
    #
    # err$tipo_muestreo <- check_variable_with_master(catches, "COD_TIPO_MUE")
    #
    # err$false_MT1 <- check_false_mt1(catches, lengths_sampled)
    #
    # err$false_MT2 <- check_false_mt2(catches, lengths_sampled)
    #
    # err$no_mixed_as_mixed <- check_no_mixed_as_mixed(lengths_sampled)
    #
    # err$mixed_as_no_mixed <- check_mixed_as_no_mixed(catches)
    #
    # err$estrato_rim_prescriptions <- checkVariableWithRimMt2PrescriptionsPost(catches, "ESTRATO_RIM")
    # err$puerto_prescriptions <- checkVariableWithRimMt2PrescriptionsPost(catches, "COD_PUERTO")
    # err$origen_prescriptions <- checkVariableWithRimMt2PrescriptionsPost(catches, "COD_ORIGEN")
    # err$arte_prescriptions <- checkVariableWithRimMt2PrescriptionsPost(catches, "COD_ARTE")
    # err$metier_dcf_prescriptions <- checkVariableWithRimMt2PrescriptionsPost(catches, "METIER_DCF")
    # err$caladero_dcf_prescriptions <- checkVariableWithRimMt2PrescriptionsPost(catches, "CALADERO_DCF")
    #
    # err$empty_fields_in_variables_catches <- emptyFieldsInVariables(catches, "RIM_CATCHES")
    # err$empty_fields_in_variables_lengths <- emptyFieldsInVariables(lengths_sampled, "RIM_LENGTHS")
    #
    # # ---- IN HEADER ----
    #
    # err$errors_mt2b_rim_stratum <- checkMt2bRimStratum(catches)
    #
    # err$coherence_estrato_rim_gear <- coherenceEstratoRimGear(catches, "RIM")
    #
    # err$coherence_estrato_rim_origin <- checkCoherenceEstratoRimOrigin(catches, "RIM")
    #
    # err$coherence_rim_mt2_prescriptions <- coherenceRimMt2PrescriptionsPost(catches)
    #
    # err$no_ships <- checkNoShips(catches)
    #
    # err$number_of_rejections <- numberOfRejections(catches)
    #
    # err$errors_countries_mt1 <- check_foreing_ships_MT1(catches)
    #
    # err$errors_countries_mt2 <- check_foreing_ships_MT2(catches)
    #
    # err$multiple_ship_code <- checkMultipleShipCode(catches)
    #
    #
    # ##### TO DO: ADD CHECKING SHIPS WITH SIRENO FILES
    #
    #
    # err$errors_ships_not_in_cfpo <-shipsNotInCFPO(catches)
    #
    # # no_en_cfpo <- err$errors_ships_not_in_cfpo %>%
    # #   filter(!grepl("^8\\d{5}",COD_BARCO) & COD_BARCO != 0) %>%
    # #   select(COD_BARCO, BARCO, COD_PUERTO, PUERTO)%>%
    # #   unique()
    #
    # err$errors_ships_not_registered <- shipsNotRegistered(catches)
    #
    # err$errors_multiple_estrato_rim <- checkMultipleEstratoRIM(catches)
    #
    # err$errors_multiple_arte <- checkMultipleGear(catches)
    #
    # err$errors_multiple_puerto <- checkMultiplePort(catches)
    #
    # err$errors_num_barcos_pareja <- checkShipsPairBottomTrawl(catches)
    #
    # err$estrategia <- checkStrategy(catches)
    #
    # err$multiple_tipo_muestreo <- multipleTypeSample(catches)
    #
    # err$tiempo_transcurrido <- check_elapsed_days(catches)
    #
    # err$checkSameTripInVariousPorts <- checkSameTripInVariousPorts(catches)
    #
    # err$checkSampleInCharge <- checkVariableFilled(catches, "RESPONSABLE_MUESTREO")
    #
    # err$shipWhithoutCODSGPM <- shipWhithoutCODSGPM(catches)

    # This errors must be used only in annual, when the Fishing Ground and DCF
    # Metier is filled:
    err$coherenceDCFFishingGroundRimStratumOrigin <- coherenceDCFFishingGroundRimStratumOrigin(
      catches
    )
    err$coherenceDCFMetierRimStratumOrigin <- coherenceDCFMetierRimStratumOrigin(
      catches
    )
    err$emptyFishingGround <- checkEmptyValuesInVariables(
      catches,
      "CALADERO_DCF",
      "catches"
    )
    err$emptyMetier <- checkEmptyValuesInVariables(
      catches,
      "METIER_DCF",
      "catches"
    )
    err$errors_cod_id <- checkCodId(muestreos_up$catches)

    # ---- IN SPECIES ----

    # err$mixed_species_category <- mixedSpeciesInCategory(catches_in_lengths)
    #
    # err$not_allowed_sampled_species <- notAllowedSampledSpecies(catches)
    #
    # err$sampled_species_doubtful <- doubtfulSampledSpecies(catches)
    #
    # err$not_allowed_category_species <- notAllowedCategorySpecies(catches_in_lengths)
    #
    # err$doubtful_category_species <- doubtfulCategorySpecies(catches_in_lengths)
    #
    # err$sexes_with_same_sampled_weight <- sexesWithSameSampledWeight(catches_in_lengths)
    #
    # err$categories_with_repeated_sexes <- categoriesWithRepeatedSexes(catches_in_lengths)
    #
    # err$lenghts_weights_sample <- checkTALL.PESO(catches)
    #
    # err$no_sexed_species <- checkNoSexedSpecies(lengths_sampled)
    #
    # err$sexed_species <- checkSexedSpecies(lengths_sampled)
    #
    # err$taxonomic_specie_confusion <- taxonomicSpecieConfusion(catches, catches_in_lengths)
    #
    # err$new_species_sampled <- checkNewSpeciesSampled(catches_in_lengths)
    #
    # TODO: FIND A BETTER WAY TO CHECK THIS, WHICH ADD THE SPECIES NAME
    # err$a3CodeFilled <- checkVariableFilled(catches, "A3_ESP_MUE")
    #
    #
    # # ---- IN WEIGHTS ----
    #
    # err$same_sampled_weight <- allCategoriesWithSameSampledWeights(catches_in_lengths)
    #
    # err$sampled_weight_zero <- weightSampledZeroWithLengthsSampled(catches_in_lengths)
    #
    # err$weight_landed_zero <- weightLandedZero(catches)
    #
    # err$weight_sampled_without_length_sampled <- weightSampledWithoutLengthsSampled(catches_in_lengths)
    #
    # err$pes_mue_desem_zero <- pesMueDesemZero(catches_in_lengths)
    #
    # err$especies_con_categorias_igual_peso_desembarcado <- speciesWithCategoriesWithSameWeightLanding(catches)
    #
    # err$sop_zero <- sopZero(catches_in_lengths)
    #
    # err$sop_greater_pes_mue_vivo <- sopGreaterPesMueVivo(catches_in_lengths)
    #
    # err$sop_mayor_peso_vivo <- sopGreaterPesVivo(catches_in_lengths)
    #
    # err$pes_mue_desem_mayor_pes_desem <- pesMueDesemGreaterPesDesem(catches_in_lengths)
    #
    # TODO: FIND A BETTER WAY TO CHECK THIS, WHICH ADD THE SPECIES NAME
    # err$a3CodeFilled <- checkVariableFilled(catches_in_lengths, "A3_ESP_CAT")
    #
    # err$categories99NotInMt2b <- categories99NotInMt2b(catches)
    #
    # # comment in annual:
    # # err$capturas_percentil_99 <- checkCatchesP99(catches)
    #
    #
    # ---- IN LENGTHS ----
    # err$all_categories_measured <- allCategoriesMeasured(catches, lengths_sampled)
    #
    # err$check_cm_measures <- checkCmMeasures(lengths_sampled)
    #
    # err$check_half_cm_measures <- checkHalfCmMeasures(lengths_sampled)
    #
    # # comment in annual:
    # # err$with_historical_size_range <- checkRangeInHistorical(muestreos_up$lengths)
    #
    # # comment in annual:
    # # err$size_range <- checkSizeRangeByFishingGround(muestreos_up$lengths)
    # # err$g1SpeciesNotMeasured <- g1SpeciesNotMeasured(catches, lengths_sampled)
    # # err$g2SpeciesNotMeasured <- g2SpeciesNotMeasured(catches, lengths_sampled)
    #
    # # ---- COD_ID ----
    # # This check is useful in the annual review. When the data is dumped in
    # # SIRENO, COD_ID is automatically filled. But, if later someone add a new
    # # sample, the COD_ID doesn't fill and is saved as empty.
    # err$cod_id_filled_catches <- checkCodId(catches)
    # err$cod_id_filled_catches_in_lengths <- checkCodId(catches_in_lengths)
    # err$cod_id_filled_lengths <- checkCodId(lengths_sampled)
    
    # ---- CHEQUEADO ----
    # This check is useful in the annual review if any trip was
    # not checked during the year
    # err$unchecked_trip <- tripIsChecked(catches)

    # ---- COMBINE ERRORS ----

    # separated by influence area
    combined_errors <- formatErrorsList(errors_list = err, separate_by_ia = T)

    return(combined_errors)
  })
}
