#### ---------------------------------------------------------------------------
#### Check monthly data recorded in SIRENO
#### 
#### Return xls or upload to google docs files with errors detected by influence
#### area
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### files required: especies_no_permitidas.csv,
#### revision_volcado_functions.R, report 'tallas por UP' from SIRENO
####
#### To install (with devtools library loaded):
#### install_github("Eucrow/revision-volcado.R")
#### ---------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# #### INSTRUCTIONS ############################################################
# ------------------------------------------------------------------------------

# To use this scritp:

# - Make sure the file 'revision_volcado_functions.R' is located in the same
# directory that this file.
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure report files tallas_x_up from SIRENO are in PATH_FILES.
# - Create a directory called 'errors' inside PATH_FILES directory
# - Choose the way to export in the "EXPORT ERRORS" section of this script.Uncomment
# the interested way. It's available in xlsx file or upload directly
# to google drive. In this case an account and password is required, and a token
# is automatically generated.
# - If xlsx option is choosen to export files, make sure a directory "errors" is
# in PATH_FILENAME path
# - Run all the script
# - A file by influence area is generated in "errors" directory.

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# YOU HAVE ONLY TO CHANGE THIS VARIABLES 

# PATH_FILES <- "F:/misdoc/sap/revision volcado/datos/2017/anual_oab"
PATH_FILES <- "F:/misdoc/sap/revision volcado/datos/2018/2018_12"
ERRORS_SUBDIRECTORY <- "errors"
FILENAME_DES_TOT <- "IEOUPMUEDESTOTMARCO_2018_12.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALMARCO_2018_12.TXT"
FILENAME_TAL <- "IEOUPMUETALMARCO_2018_12.TXT"

MONTH <- 12 # month in numeric or FALSE for a complete year 
YEAR <- "2018"

# only if the file must be uploaded to google drive
GOOGLE_DRIVE_PATH <- "/equipo muestreos/revisión_volcado/2018/2018_correcciones_para_sups/"

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# #### PACKAGES ################################################################
# ------------------------------------------------------------------------------

library(plyr)
library(dplyr) #arrange_()
# library(tools) #file_path_sans_ext()
library(devtools)

# ---- install googlesheets from github
# install_github("jennybc/googlesheets")
# library(googlesheets)
# uppressMessages(library(dplyr)) #What is suppressMessages????


# ---- install sapmuebase from local
# remove.packages("sapmuebase")
# .rs.restartR()
# install_github("Eucrow/sapmuebase")
  # install("F:/misdoc/sap/sapmuebase")
library(sapmuebase)

# ---- install googledrive package from github
# devtools::install_github("tidyverse/googledrive")
library(googledrive)


# ------------------------------------------------------------------------------
# #### SET WORKING DIRECTORY ###################################################
# ------------------------------------------------------------------------------

setwd("F:/misdoc/sap/revision volcado/revision_volcado_R/")

# ------------------------------------------------------------------------------
# #### GLOBAL VARIABLES ########################################################
# ------------------------------------------------------------------------------

# list with all errors found in dataframes:
ERRORS <- list()


# list with the common fields used in all tables
BASE_FIELDS <- c("COD_ID", "COD_PUERTO", "PUERTO", "LOCODE", "FECHA_MUE", "COD_BARCO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "TIPO_MUE")

# useful paths
PATH_ERRORS <- paste(PATH_FILES,"/errors", sep="")
PATH_BACKUP <- paste(PATH_ERRORS, "/backup", sep="")

# month as character
MONTH_AS_CHARACTER <- sprintf("%02d", MONTH)

# read especies_sujetas_a_posible_confusión_taxonómica.csv file
ESP_TAXONOMIC_CONFUSION <- read.csv(
  "especies_sujetas_a_posible_confusión_taxonómica.csv",
  sep = ";",
  fileEncoding = "UTF-8",
  colClasses = c("factor","factor","factor","factor","factor","factor","character","character"))


# ------------------------------------------------------------------------------
# #### FUNCTIONS ###############################################################
# ------------------------------------------------------------------------------
# All the functions required in this script are located in
# revision_volcado_functions.R file.
source('revision_volcado_functions.R')


# ------------------------------------------------------------------------------
# #### IMPORT DATA #############################################################
# ------------------------------------------------------------------------------

#PONER EN CASTELLANO
#read the mixed species dataset

mixed_species <- especies_mezcla

#read the no mixed species dataset
sampled_spe_no_mixed <- especies_no_mezcla

###obtain the not allowed species dataset
NOT_ALLOWED_SPECIES <- read.csv("especies_no_permitidas.csv", fileEncoding = "UTF-8")

### obtain the cfpo
CFPO <- cfpo2017
  # ignore useless columns
  CFPO <- CFPO[,c("CODIGO_BUQUE", "ESTADO")]
  
  
# ------------------------------------------------------------------------------ 
# #### IMPORT muestreos_UP files ###############################################
# ------------------------------------------------------------------------------

muestreos_up <- importRIMFiles(
  des_tot = FILENAME_DES_TOT,
  des_tal = FILENAME_DES_TAL,
  tal = FILENAME_TAL,
  path = PATH_FILES,
  by_month = MONTH)
  
# catches <- importRIMCatches(FILENAME_DES_TOT, path= PATH_FILES)
# catches_in_lengths <- importRIMCatchesInLengths(FILENAME_DES_TAL, path= PATH_FILES)
# prueba_LENGTHS <- importRIMLengths(FILENAME_TAL, path= PATH_FILES)
 

#isolate dataframes
catches <- muestreos_up$catches
catches_in_lengths <- muestreos_up$catches_in_lengths
lengths <- muestreos_up$lengths

# ------------------------------------------------------------------------------
# #### SEARCHING ERRORS ########################################################
# ------------------------------------------------------------------------------

check_them_all <- function () {
  
  tryCatch({
    
    err <- list()
    
    # ---- REPEATED IN IPDTOSIRENO ----
    
    err$estrato_rim <- check_variable_with_master(catches, "ESTRATO_RIM")
    
    err$puerto <- check_variable_with_master(catches, "COD_PUERTO")
    
    err$arte <- check_variable_with_master(catches, "COD_ARTE")
    
    err$origen <- check_variable_with_master(catches, "COD_ORIGEN")
    
    err$procedencia <- check_variable_with_master(catches, "PROCEDENCIA")
    
    err$tipo_muestreo <- check_variable_with_master(catches, "COD_TIPO_MUE")
    
    err$false_MT1 <- check_false_mt1()
    
    err$false_MT2 <- check_false_mt2()
    
    err$no_mixed_as_mixed <- check_no_mixed_as_mixed()
    
    err$mixed_as_no_mixed <- check_mixed_as_no_mixed()
    
    # ---- IN HEADER ----
    
    err$errors_mt2b_rim_stratum <- checkMt2bRimStratum()
    
    err$coherence_estrato_rim_gear <- coherenceEstratoRimGear(catches)
    
    err$coherence_estrato_rim_origin <- checkCoherenceEstratoRimOrigin()
    
    err$number_of_ships <- numberOfShips()
    
    err$number_of_rejections <- numberOfRejections()
    
    err$errors_countries_mt1 <- check_foreing_ships_MT1(catches)
    
    err$errors_countries_mt2 <- check_foreing_ships_MT2(catches)
    
    
    ##### TO DO: ADD CHECKING SHIPS WITH SIRENO FILES
    
    err$errors_ships_not_in_cfpo <-shipsNotInCFPO(catches)
    
    no_en_cfpo <- err$errors_ships_not_in_cfpo %>%
                    filter(!grepl("^8\\d{5}",COD_BARCO) & COD_BARCO != 0) %>%
                    select(COD_BARCO, BARCO, COD_PUERTO, PUERTO)%>%
                    unique()
            
    err$errors_ships_not_registered <- shipsNotRegistered(catches)
    
    err$errors_multiple_estrato_rim <- checkMultipleEstratoRIM()
    
    err$errors_multiple_arte <- checkMultipleGear()
    
    err$errors_multiple_puerto <- checkMultiplePort()
    
    err$errors_num_barcos_pareja <- checkShipsPairBottomTrawl()
    
    err$estrategia <- checkStrategy()
    
    err$multiple_tipo_muestreo <- multipleTypeSample()
    
    err$tiempo_transcurrido <- check_elapsed_days()
    
    err$checkSameTripInVariousPorts <- checkSameTripInVariousPorts()
    
    err$checkSampleResponsible <- checkVariableFilled(catches, "RESPONSABLE_MUESTREO")
    
    # ---- IN SPECIES ----
    
    err$mixed_species_category <- mixedSpeciesInCategory()
          
    err$not_allowed_sampled_species <- notAllowedSampledSpecies()
    
    err$sampled_species_doubtful <- doubtfulSampledSpecies()
    
    err$not_allowed_category_species <- notAllowedCategorySpecies()
    
    err$doubtful_category_species <- doubtfulCategorySpecies()
    
    err$sexes_with_same_sampled_weight <- sexesWithSameSampledWeight()
    
    err$categories_with_repeated_sexes <- categoriesWithRepeatedSexes()
    
    err$lenghts_weights_sample <- checkTALL.PESO()
    
    err$no_sexed_species <- checkNoSexedSpecies()
    
    err$sexed_species <- checkSexedSpecies()
    
    err$taxonomic_specie_confusion <- taxonomicSpecieConfusion()
    
    err$a3CodeFilled <- checkVariableFilled(catches, "A3_ESP_MUE")
    
    
    # ---- IN WEIGHTS ----
    
    err$same_sampled_weight <- allCategoriesWithSameSampledWeights()
      
    err$sampled_weight_zero <- weightSampledZeroWithLengthsSampled()
        
    err$weight_landed_zero <- weightLandedZero()
    
    err$weight_sampled_without_length_sampled <- weightSampledWithoutLengthsSampled()
        
    err$pes_mue_desem_zero <- pesMueDesemZero()
    
    err$especies_con_categorias_igual_peso_desembarcado <- speciesWithCategoriesWithSameWeightLanding()
            
    err$sop_zero <- sopZero() 
         
    err$sop_greater_pes_mue_vivo <- sopGreaterPesMueVivo()
    
    err$sop_mayor_peso_vivo <- sopGreaterPesVivo()
    
    err$pes_mue_desem_mayor_pes_desem <- pesMueDesemGreaterPesDesem()
    
    err$capturas_percentil_97 <- checkCatchesP97()
    
    err$a3CodeFilled <- checkVariableFilled(catches_in_lengths, "A3_ESP_CAT")
    
    
    # ---- IN LENGTHS ----
    
    err$rango_tallas <- checkSizeRange()
    
    # ---- COD_ID ----
    # This check is usefull in the anual review. When the data is dumped in
    # SIRENO, COD_ID is automatically filled. But, if later someone add a new
    # sample, the COD_ID doesn't fill and is saved as empty.
    err$cod_id_filled_catches <- checkCodId(catches)
    err$cod_id_filled_catches_in_lengths <- checkCodId(catches_in_lengths)
    err$cod_id_filled_lengths <- checkCodId(lengths)
    
    # ---- COMBINE ERRORS ----
    
    combined_errors <- formatErrorsList(errors_list = err, separate_by_ia = T)
    
    return(combined_errors)
    
  })
  
}

errors <- check_them_all()


# ------------------------------------------------------------------------------
# #### EXPORT ERRORS ###########################################################
# ------------------------------------------------------------------------------

# Uncomment the way to export errors:

    # one month

    # exportListToCsv(combined_errors, suffix = paste0(YEAR,"_",MONTH_AS_CHARACTER), separation = "_")

    # exportListToXlsx(errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
    # exportListToXlsx(errors, suffix = paste0("errorsbbb", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
    
    exportErrorsList(errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
    
    #exportListToGoogleSheet(errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_" )
    
    # exportListToGoogleSheet(errors, suffix = paste0("errors", "_", YEAR), separation = "_" ) 
    # a complete year 

    # exportListToXlsx(combined_errors, suffix = paste0("errors", "_", YEAR), separation = "_")

    # exportListToGoogleSheet(combined_errors, suffix = paste0("errors", "_", YEAR), separation = "_")

# ------------------------------------------------------------------------------    
# #### CHECK CODE_ID ###########################################################
# This check is not for send to the sups, so it's out the ERRORS dataframe

# ------------------------------------------------------------------------------

# errors_cod_id <- checkCodId()


# ------------------------------------------------------------------------------    
# #### MAKE A BACKUP
# ------------------------------------------------------------------------------
    # backup_files()



    


