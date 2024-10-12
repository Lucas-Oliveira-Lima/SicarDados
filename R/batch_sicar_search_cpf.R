#' Função para aplicar consulta de imóveis a uma lista de CPFs
#'
#' @param cpf_list Lista de CPFs a serem consultados.
#' @return Dataframe com os resultados das consultas de todos os CPFs.
#' @export
batch_sicar_search <- function(cpf_list) {
  resultados <- purrr::map_dfr(cpf_list, sicar_search_cpf)
  return(resultados)
}
