#' Função para consultar imóveis em lote vinculados a uma lista de códigos CAR
#'
#' @param car_codes Vetor de códigos CAR a serem consultados.
#' @return Dataframe com os resultados das consultas de todos os códigos CAR.
#' @export
batch_sicar_search_car_code <- function(car_codes) {
  # Criar a barra de progresso
  pb <- progress::progress_bar$new(
    total = length(car_codes),
    format = "  Executando [:bar] :percent em :elapsed",
    clear = FALSE
  )
  
  # Aplicar a função de consulta a cada código CAR
  resultados <- purrr::map_dfr(car_codes, sicar_search_car_code, pb = pb)
  
  return(resultados)
}
