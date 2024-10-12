#' Função para processar vários imóveis com base em uma lista de CARs
#'
#' @param car_codes Vetor de códigos CAR
#' @return Dataframe consolidado com os dados de todos os imóveis
#' @export
batch_extract_demonstrativo <- function(car_codes) {
  # Processar cada imóvel da lista de CARs e consolidar em um dataframe
  df_consolidado <- dplyr::bind_rows(lapply(car_codes, processar_imovel))

  # Criar pasta 'data' se não existir
  if (!dir.exists("data")) {
    dir.create("data")
  }

  # Salvar o dataframe consolidado em um arquivo Excel
  openxlsx::write.xlsx(df_consolidado, file = "data/db_consolidado_infos_demonstrativo_car.xlsx", rowNames = FALSE)

  return(df_consolidado)
}
