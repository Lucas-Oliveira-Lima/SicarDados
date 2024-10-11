#' Função para formatar a string do código CAR
#'
#' @param input_string String de código CAR a ser formatada.
#' @return Retorna o código CAR formatado com pontos.
#' @export
formatar_string <- function(input_string) {
  inicio <- substr(input_string, 1, 11)
  restante <- substr(input_string, 12, nchar(input_string))
  restante_formatado <- gsub("(.{4})", "\\1.", restante)
  restante_formatado <- sub("\\.$", "", restante_formatado)
  formatted_string <- paste0(inicio, restante_formatado)
  
  return(formatted_string)
}

#' Função para consultar imóveis vinculados a um código CAR no SICAR
#'
#' @param codigo Código CAR para consulta.
#' @param pb (opcional) Barra de progresso da consulta.
#' @return Um dataframe com os dados de imóveis vinculados ao código CAR.
#' @export
sicar_search_car_code <- function(codigo, pb = NULL) {
  # Atualizar a barra de progresso, se fornecida
  if (!is.null(pb)) pb$tick()
  
  # Formatar o código usando a função de formatação
  codigo_formatado <- formatar_string(codigo)
  
  # URL base da consulta
  url_base <- "http://car.semas.pa.gov.br/site/consulta/imoveis/CODIGO/list?filtro="
  
  # Construir a URL com o código formatado
  url <- paste0(url_base, codigo_formatado, "&pagina=1")
  
  # Verificar se a URL está correta antes de fazer a requisição
  if (!grepl("^http://", url)) {
    warning(paste("URL mal formatada:", url))
    return(NULL)
  }
  
  # Fazer a requisição GET
  resposta <- httr::GET(url)
  
  # Verificar se a requisição foi bem-sucedida
  if (httr::http_error(resposta)) {
    warning(paste("Erro ao consultar código:", codigo_formatado))
    return(NULL)
  }
  
  # Extrair conteúdo do GeoJSON
  conteudo <- httr::content(resposta, as = "text", encoding = "UTF-8")
  geojson <- jsonlite::fromJSON(conteudo, simplifyVector = FALSE)
  
  # Verificar se o GeoJSON tem a estrutura esperada
  if (!is.list(geojson) || !all(c("features") %in% names(geojson))) {
    warning(paste("GeoJSON não possui estrutura esperada para o código:", codigo_formatado))
    return(NULL)
  }
  
  # Extrair as propriedades de cada feature do GeoJSON
  if (length(geojson$features) == 0) {
    warning(paste("GeoJSON não contém features para o código:", codigo_formatado))
    return(NULL)
  }
  
  propriedades <- purrr::map_df(geojson$features, ~ {
    prop <- .x$properties
    data.frame(
      codigo = codigo_formatado,
      id = prop$id,
      codigo = prop$codigo,
      protocolo = prop$protocolo,
      area = prop$area,
      nome = prop$nome,
      status = prop$status,
      stringsAsFactors = FALSE
    )
  })
  
  return(propriedades)
}
