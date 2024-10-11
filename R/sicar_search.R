#' Função para consultar imóveis vinculados a um CPF no sistema SICAR
#'
#' @param cpf Número do CPF para consulta (11 dígitos).
#' @return Um dataframe com os dados de imóveis vinculados ao CPF.
#' @export
sicar_search_cpf <- function(cpf) {
  # URL base da consulta
  url_base <- "http://car.semas.pa.gov.br/site/consulta/imoveis/PROPRIETARIO_POSSUIDOR/list?filtro="
  
  # Construir a URL com o CPF
  url <- paste0(url_base, cpf, "&pagina=1")
  
  # Fazer a requisição GET
  resposta <- httr::GET(url)
  
  # Verificar se a requisição foi bem-sucedida
  if (httr::http_error(resposta)) {
    warning(paste("Erro ao consultar CPF:", cpf))
    return(NULL)
  }
  
  # Extrair conteúdo do GeoJSON
  conteudo <- httr::content(resposta, as = "text")
  geojson <- jsonlite::fromJSON(conteudo, simplifyVector = FALSE)  # Manter a estrutura do GeoJSON como lista
  
  # Verificar se o GeoJSON tem a estrutura esperada
  if (!is.list(geojson) || !all(c("features") %in% names(geojson))) {
    warning("GeoJSON não possui estrutura esperada.")
    return(NULL)
  }
  
  # Extrair as propriedades de cada feature do GeoJSON
  if (length(geojson$features) == 0) {
    warning(paste0("GeoJSON não contém features.", cpf))
    return(NULL)
  }
  
  propriedades <- purrr::map_df(geojson$features, ~ {
    if (!is.list(.x) || !all(c("properties") %in% names(.x))) {
      warning("Feature não contém propriedades válidas.")
      return(NULL)
    }
    prop <- .x$properties
    data.frame(
      cpf = cpf,
      id = prop$id,
      codigo = prop$codigo,
      protocolo = prop$protocolo,
      area = prop$area,
      nome = prop$nome,
      status = prop$status
    )
  })
  
  # Pausar por 1 segundo antes da próxima consulta
  Sys.sleep(1)
  
  return(propriedades)
}
