#' Move a file from one directory to another
#'
#' @description
#' Safely moves a file between directories with error handling.
#'
#' @param file_name Character string. Name of the file to be moved (including extension)
#' @param origin_folder Character string. Path to the directory where the file is currently stored
#' @param destiny_folder Character string. Path to the destination directory
#'
#' @return Invisible NULL. Prints a success message if the file is moved correctly,
#'   or an error/warning message if the operation fails
#'
#' @details
#' The function uses \code{file.rename()} internally and includes error handling
#' via \code{tryCatch()}. Messages are printed to the console indicating success or failure.

move_file <- function(file_name,
                      origin_folder,
                      destiny_folder) {
  tryCatch(
    {
      file.rename(
        file.path(origin_folder, file_name),
        file.path(destiny_folder, file_name)
      )

      message(paste0("File '", file_name, "' moved to '", destiny_folder, "' correctly."))
    },
    error = function(e) {
      cat("An error occurred:", conditionMessage(e), "\n")
    },
    warning = function(w) {
      cat("A warning occurred:", conditionMessage(w), "\n")
    }
  )
}
