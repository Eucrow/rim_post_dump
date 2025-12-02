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
