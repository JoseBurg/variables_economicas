library(openxlsx)
library(dplyr)

agregados_monetarios <- function(){
  
  # Url del banco
  url <- paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/base_monetaria.xlsx?v=1695927534522"
    )
    
  
  # Descargar base de datos
  file <- tempfile(fileext = ".xlsx")
  download.file(url, file, mode = "wb")
  
  # Lectura y limpieza de datos
  base <-
    openxlsx::read.xlsx(file, startRow = 13, colNames = FALSE) |>
      setNames(
        c("year", "mes", 
          # Base monetaria restringida R
          "r_billetes_monedas", "r_depositos_transf", 
          "r_valores", "base_restringida",
          # Base monetaria ampliada    A
          "a_depositos_transf",
          "a_otros_depositos", "a_valores", "a_otros_depositos_mn",
          "a_otros_depositos_me", "a_otros_valores_mn", "a_otros_valores_me",
          "a_otros_depositos_valores_mn", "a_otros_depositos_valores_me",
          "base_Ampliada")) |>
      as_tibble() |>
      dplyr::filter(!is.na(mes))
  
  # Cantidad de filas para crear el periodo
  dim <- nrow(base)
  
  # Ajustando los datos
    base |> 
      dplyr::mutate(
        mes = recode(mes, 'Apr' = 'Abr'),
        across(-mes, as.numeric),
        periodo = seq(lubridate::ymd("2001-12-01"), length.out = dim, by = "month"))  |> 
      dplyr::select(periodo, year, mes, everything())
    
    
}



