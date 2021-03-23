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

# To use this script:

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

# PATH_FILES <- "F:/misdoc/sap/rim_post_dump/datos/2019/2019_01_to_07"
# PATH_FILES <- "C:/Users/Marco IEO/Desktop/rim_post_dump/datos/2019/2019_08"
# PATH_FILES <- file.path(getwd(), "datos/2020/2020_01")
PATH_FILES <- "C:/Users/ieoma/Desktop/sap/rim_post_dump/datos/2021/2021_01"

ERRORS_SUBDIRECTORY <- "errors"
FILENAME_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALMARCO.TXT"
FILENAME_TAL <- "IEOUPMUETALMARCO.TXT"

MONTH <- FALSE # month in numeric or FALSE for a complete year
# MONTH <- FALSE # month in numeric or FALSE for a complete year
YEAR <- "2021"

# only if the file must be uploaded to google drive
# GOOGLE_DRIVE_PATH <- "/equipo muestreos/revision_volcado/2020/2020_correcciones_para_sups/"

# cfpo to use in the script (must be included in sapmuebase package)
cfpo_to_use <- "cfpo2019"

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
# install("F:/misdoc/sap/sapmuebase")
# install("C:/Users/ieoma/Desktop/sap/sapmuebase")

# ---- install sapmuebase from github
# remove.packages("sapmuebase")
# .rs.restartR()
# install_github("eucrow/sapmuebase")

library(sapmuebase)

# ---- install googledrive package from github
# devtools::install_github("tidyverse/googledrive")
# library(googledrive)


# ------------------------------------------------------------------------------
# #### SET WORKING DIRECTORY ###################################################
# ------------------------------------------------------------------------------

# setwd("F:/misdoc/sap/revision volcado/revision_volcado_R/")
# setwd("C:/Users/Marco IEO/Google Drive/revision_volcado/revision_volcado_R/")

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
MONTH_AS_CHARACTER <- ifelse(isFALSE(MONTH), "", sprintf("%02d", MONTH))

# ------------------------------------------------------------------------------
# #### FUNCTIONS ###############################################################
# ------------------------------------------------------------------------------
# All the functions required in this script are located in
# revision_volcado_functions.R file.
source('rim_post_dump_functions.R')

# function to check the rim files:
source('rim_check.R')

# function to check the oab files:
source('oab_check.R')


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

# read especies_sujetas_a_posible_confusion_taxonómica.csv file
ESP_TAXONOMIC_CONFUSION <- read.csv(
  "especies_sujetas_a_posible_confusion_taxonomica.csv",
  sep = ";",
  colClasses = c("factor","factor","factor","factor","factor","factor","character","character"))

### obtain the cfpo
CFPO <- get(cfpo_to_use)
  # ignore useless columns
  CFPO <- CFPO[,c("CODIGO_BUQUE", "ESTADO")]
  
  
# ------------------------------------------------------------------------------ 
# #### IMPORT muestreos_UP files ###############################################
# ------------------------------------------------------------------------------

muestreos_up <- importRIMFiles(
  catches = FILENAME_DES_TOT,
  catches_in_lengths = FILENAME_DES_TAL,
  lengths = FILENAME_TAL,
  path = PATH_FILES,
  by_month = MONTH)


# catches <- importRIMCatches(FILENAME_DES_TOT, path= PATH_FILES)
# catches_in_lengths <- importRIMCatchesInLengths(FILENAME_DES_TAL, path= PATH_FILES)
# lengths_sampled <- importRIMLengths(FILENAME_TAL, path= PATH_FILES)

# filter only type 6 samples
# muestreos_up$catches <- muestreos_up$catches[muestreos_up$catches$COD_TIPO_MUE == 6, ]
# muestreos_up$catches_in_lengths <- muestreos_up$catches_in_lengths[muestreos_up$catches_in_lengths$COD_TIPO_MUE == 6, ]
# muestreos_up$lengths <- muestreos_up$lengths[muestreos_up$lengths$COD_TIPO_MUE == 6, ]

# TO DO: check that muestreos_up is not empty --> sometimes happened because the
# directory or month hasn't been changed.
 
#isolate dataframes
# catches <- muestreos_up$catches
# catches_in_lengths <- muestreos_up$catches_in_lengths
# lengths <- muestreos_up$lengths

# ------------------------------------------------------------------------------
# #### SEARCHING ERRORS ########################################################
# ------------------------------------------------------------------------------

# Check rim data:
#   - sampled type 1, MT1A
#   - sampled type 2, MT2A
#   - sampled type 6, MT3  

errors <- rim_check(muestreos_up) 

# Check oab data dumped in rim:
#   - sampled type 4, MT2B
# errors <- oab_check(muestreos_up)
  

# ------------------------------------------------------------------------------
# #### EXPORT ERRORS ###########################################################
# ------------------------------------------------------------------------------

# Uncomment the way to export errors:

    # one month
    # exportListToCsv(errors, suffix = paste0(YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
    # exportErrorsList(errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
  
    # WARNING: the google drive upload doesn't work:
    # Error in add_id_path(nodes, root_id = root_id, leaf = leaf) : 
    # !anyDuplicated(nodes$id) is not TRUE 
    # devtools::install_github("tidyverse/googledrive")
    # exportListToGoogleSheet(errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_" )

    # a complete year 
    exportErrorsList(errors, suffix = paste0("errors", "_", YEAR), separation = "_")
    # exportListToGoogleSheet(errors, suffix = paste0("errors", "_", YEAR), separation = "_" ) 

# ------------------------------------------------------------------------------    
# #### CHECK CODE_ID ###########################################################
# This check is not for send to the sups, so it's out the ERRORS dataframe

# ------------------------------------------------------------------------------

# errors_cod_id <- checkCodId()


# ------------------------------------------------------------------------------    
# #### MAKE A BACKUP
# ------------------------------------------------------------------------------
    # backup_files()


