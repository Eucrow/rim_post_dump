#### Check monthly data recorded in SIRENO
####
#### Return xls or upload to google docs files with errors detected by influence
#### area
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
#### version: 2.0


# INSTRUCTIONS -----------------------------------------------------------------

# To use this script:
# - Make sure the file 'revision_volcado_functions.R' is located in the same
# directory that this file.
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure report files tallas_x_up from SIRENO are in PATH_FILES.
# - Create a directory called 'errors' inside PATH_FILES directory
# - Choose the way to export in the "EXPORT ERRORS" section of this script.
# Uncomment the interested way. It's available in xlsx file or upload directly
# to google drive. In this case an account and password is required, and a token
# is automatically generated.
# - If xlsx option is choosen to export files, make sure a directory "errors" is
# in PATH_FILENAME path
# - Run all the script
# - A file by influence area is generated in "errors" directory.

# YOU HAVE ONLY TO CHANGE THIS VARIABLES ---------------------------------------

# PATH_FILES <- file.path(getwd(), "datos/2022/2022_10_b")
# PATH_FILES <- "C:/Users/ieoma/Desktop/sap/rim_post_dump/datos/2021/2021_annual"

ERRORS_SUBDIRECTORY <- "errors"
FILENAME_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"
FILENAME_DES_TAL <- "IEOUPMUEDESTALMARCO.TXT"
FILENAME_TAL <- "IEOUPMUETALMARCO.TXT"

# MONTH: 1 to 12, or vector with month in numbers
MONTH <- 1
# MONTH <- c(1:12)
# Use only in case MONTH is a vector of months: suffix to add to path:
suffix_multiple_months <- ""

# Suffix to add at the end of the export filename
suffix <- ""

# YEAR
YEAR <- 2023

# only if the file must be uploaded to google drive
# GOOGLE_DRIVE_PATH <- "/equipo muestreos/revision_volcado/2020/2020_correcciones_para_sups/"

# cfpo to use in the script
# cfpo_to_use <- "CFPO_2021.csv"
cfpo_to_use <- "CFPO 2022 DEF Marco.xlsx"


# PACKAGES ---------------------------------------------------------------------

library(plyr)
library(dplyr) #arrange_()
# library(tools) #file_path_sans_ext()
library(devtools)

# ---- install googlesheets from github
# install_github("jennybc/googlesheets")
# library(googlesheets)
# suppressMessages(library(dplyr)) #What is suppressMessages????

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


# FUNCTIONS --------------------------------------------------------------------
# All the functions required in this script are located in
# revision_volcado_functions.R file.
source('rim_post_dump_functions.R')

# function to check the rim files:
source('rim_check.R')

# function to check the annual rim files:
source('rim_check_annual.R')

# function to check the oab files:
source('oab_check.R')


# SET WORKING DIRECTORY --------------------------------------------------------

# setwd("F:/misdoc/sap/revision volcado/revision_volcado_R/")
# setwd("C:/Users/Marco IEO/Google Drive/revision_volcado/revision_volcado_R/")


# GLOBAL VARIABLES -------------------------------------------------------------

# list with all errors found in dataframes:
ERRORS <- list()

PATH_FILES <- createPathFiles(MONTH, YEAR, suffix_multiple_months)




# list with the common fields used in all tables
BASE_FIELDS <- c("COD_ID", "COD_PUERTO", "PUERTO", "LOCODE", "FECHA_MUE",
                 "COD_BARCO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE",
                 "TIPO_MUE")

# path to store errors files
PATH_ERRORS <- file.path(PATH_FILES, "errors")
# path to store files as backup
PATH_BACKUP <- file.path(PATH_FILES, "backup")

# month as character
# MONTH_AS_CHARACTER <- ifelse(isFALSE(MONTH), "", sprintf("%02d", MONTH))
# month as character in case of monthly check
MONTH_AS_CHARACTER <- createMonthAsCharacter()
# if (length(MONTH) == 1 && MONTH %in% seq(1:12)){
#   MONTH_AS_CHARACTER <- ifelse(isFALSE(MONTH), "", sprintf("%02d", MONTH))
# } else if (length(MONTH) > 1 & all(MONTH %in% seq(1:12))) {
#   MONTH_AS_CHARACTER <- suffix_multiple_months
# } else if (MONTH == "annual") {
#   MONTH_AS_CHARACTER <- "annual"
# } else {
#   stop("Is there any error in the MONTH variable?")
# }

# path to shared folder
PATH_SHARE_ERRORS <- file.path("C:/Users/ieoma/SAP_MUE/SAP_RIM - RIM_data_review - RIM_data_review", YEAR, paste0(YEAR, "_", MONTH_AS_CHARACTER))


# files to backup
FILES_TO_BACKUP <- c("rim_post_dump.R",
                     "rim_post_dump_functions.R",
                     "rim_check.R",
                     "oab_check.R",
                     "especies_sujetas_a_posible_confusion_taxonomica.csv",
                     "especies_no_permitidas.csv")

createFilename <- function(){
  filename <- paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER)

  if (suffix != ""){
    filename <- paste(filename, suffix, sep="_")
  }

  return (filename)
}

ERRORS_FILENAME <- createFilename()




# IMPORT DATA ------------------------------------------------------------------

#read the mixed species dataset

mixed_species <- especies_mezcla

#read the no mixed species dataset
sampled_spe_no_mixed <- especies_no_mezcla

###obtain the not allowed species dataset
NOT_ALLOWED_SPECIES <- read.csv("especies_no_permitidas.csv")

# read especies_sujetas_a_posible_confusion_taxonómica.csv file
ESP_TAXONOMIC_CONFUSION <- read.csv(
  "especies_sujetas_a_posible_confusion_taxonomica.csv",
  sep = ";",
  colClasses = c("factor","factor","factor","factor","factor","factor","character","character"))

### obtain the cfpo
# CFPO <- get(cfpo_to_use)

# CFPO <- read.csv2(paste0(getwd(), "/data-raw/", cfpo_to_use), sep = ";", fileEncoding = "windows-1252")
# ignore useless columns
# CFPO <- CFPO[,c("CODIGO_BUQUE", "ESTADO")]
# CFPO <- CFPO[,c("CODIGOBUQUE", "Estado.Actual")]
# colnames(CFPO) <- c("CODIGO_BUQUE", "ESTADO")

library(openxlsx)
CFPO <- read.xlsx(paste0(getwd(), "/data-raw/", cfpo_to_use), detectDates=TRUE)
CFPO <- CFPO[, c("CFR", "Matrícula", "Estado.actual")]
colnames(CFPO) <- c("CFR", "MATRICULA", "ESTADO")


# IMPORT "muestreos UP" files --------------------------------------------------

muestreos_up <- importRIMFiles(
  catches = FILENAME_DES_TOT,
  catches_in_lengths = FILENAME_DES_TAL,
  lengths = FILENAME_TAL,
  path = PATH_FILES
  # ,  by_month = MONTH
  )


# catches <- importRIMCatches(FILENAME_DES_TOT, path= PATH_FILES)
# catches_in_lengths <- importRIMCatchesInLengths(FILENAME_DES_TAL, path= PATH_FILES)
# lengths_sampled <- importRIMLengths(FILENAME_TAL, path= PATH_FILES)

# remove type 6 samples
# muestreos_up$catches <- muestreos_up$catches[muestreos_up$catches$COD_TIPO_MUE != 6, ]
# muestreos_up$catches_in_lengths <- muestreos_up$catches_in_lengths[muestreos_up$catches_in_lengths$COD_TIPO_MUE != 6, ]
# muestreos_up$lengths <- muestreos_up$lengths[muestreos_up$lengths$COD_TIPO_MUE != 6, ]

# TO DO: check that muestreos_up is not empty --> sometimes happened because the
# directory or month hasn't been changed.


# SEARCHING ERRORS -------------------------------------------------------------

# Check rim data:
#   - sampled type 1, MT1A
#   - sampled type 2, MT2A

errors <- rim_check(muestreos_up)
# errors <- rim_check_annual(muestreos_up)

errors_complete <- Reduce( function(x, y) { merge(x, y, all=TRUE)}, errors)

# Check oab data dumped in rim:
#   - sampled type 4, MT2B
# errors_oab <- oab_check(muestreos_up)


# EXPORT ERRORS ----------------------------------------------------------------

# Select the way to export errors:

    # one month
    # exportListToCsv(errors, suffix = paste0(YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
    # exportErrorsList(errors, suffix = paste0("errors", "_", YEAR,"_",MONTH_AS_CHARACTER), separation = "_")
    exportErrorsList(errors, ERRORS_FILENAME, separation = "_")
    # exportErrorsList(errors, suffix = paste0("errors", "_", YEAR,"_annual"), separation = "_")

    # a complete year
    # exportErrorsList(errors, suffix = paste0("errors", "_", YEAR), separation = "_")
    # exportErrorsList(errors_oab, suffix = paste0("errors_oab", "_", YEAR), separation = "_")
    # exportCsvSAPMUEBASE(errors_complete, "errors_complete_anual_2021.csv")


# CHECK CODE_ID ----------------------------------------------------------------
# This check is not for send to the sups, so it's out the ERRORS dataframe
# errors_cod_id <- checkCodId(muestreos_up$catches)


# SAVE FILES TO SHARED FOLDER --------------------------------------------------
copyFilesToFolder(PATH_ERRORS, PATH_SHARE_ERRORS)

# BACKUP SCRIPTS AND RELATED FILES ---------------------------------------------
# first save all files opened
rstudioapi::documentSaveAll()
# and the backup the scripts and files:
sapmuebase::backupScripts(FILES_TO_BACKUP, path_backup = PATH_BACKUP)



