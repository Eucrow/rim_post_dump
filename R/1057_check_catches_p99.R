#' Detect outliers in species catches.
#' Uses the p99_capturas_historico dataset to detect if the total catch
#' of a species in an stratum exceed the 99th percentile of historical catches.
#' @param catches Data frame with catches data
#' @return A data frame with warnings if found, NULL otherwise
#' @note Check code: 1057
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
