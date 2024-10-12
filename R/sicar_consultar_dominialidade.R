#' Função para consultar a dominialidade de um imóvel CAR a partir do código CAR
#'
#' @param codigo_car Código CAR para consulta
#' @param pb (opcional) Barra de progresso da consulta.
#' @return Um dataframe com as informações de dominialidade (nome, CPF/CNPJ, tipo, código CAR e ID).
#' @export
sicar_consultar_dominialidade <- function(codigo_car, pb = NULL) {

  # Consultar os imóveis vinculados ao código CAR usando a função sicar_search_car_code
  resultado_car <- sicar_search_car_code(codigo_car, pb)

  # Verificar se o resultado está vazio ou nulo
  if (is.null(resultado_car) || nrow(resultado_car) == 0) {
    stop("Nenhum imóvel encontrado para o código CAR fornecido.")
  }

  # URL base para consulta de dominialidade
  url_base <- "http://car.semas.pa.gov.br/site/imovel/"

  # Função auxiliar para processar cada ID
  processar_dominio <- function(codigo_car, id) {

    # Construir a URL com o ID do imóvel
    url <- paste0(url_base, id, "/dominioFichaResumida")

    # Fazer a requisição e capturar a página
    pagina <- tryCatch(httr::GET(url), error = function(e) return(NA))

    # Verificar se a requisição foi bem-sucedida
    if (inherits(pagina, "response") && httr::status_code(pagina) != 200) {
      warning(paste("Erro ao consultar o ID:", id))
      return(data.frame(codigo_car = codigo_car, id = id, nome = NA, cpf_cnpj = NA, tipo = NA, stringsAsFactors = FALSE))
    }

    # Verificar se a resposta é válida e foi recebida corretamente
    if (!inherits(pagina, "response")) {
      return(data.frame(codigo_car = codigo_car, id = id, nome = NA, cpf_cnpj = NA, tipo = NA, stringsAsFactors = FALSE))
    }

    # Extrair conteúdo HTML
    conteudo_html <- httr::content(pagina, as = "text", encoding = "UTF-8")
    pagina <- xml2::read_html(conteudo_html)

    # Extrair todas as tabelas HTML
    tabelas <- rvest::html_nodes(pagina, "table.table")

    if (length(tabelas) == 0) {
      warning(paste("Nenhuma tabela encontrada para o ID:", id))
      return(data.frame(codigo_car = codigo_car, id = id, nome = NA, cpf_cnpj = NA, tipo = NA, stringsAsFactors = FALSE))
    }

    # Processar cada tabela
    result_list <- lapply(tabelas, function(tabela) {
      linhas <- rvest::html_nodes(tabela, "tr")
      if (length(linhas) == 3) {
        nome <- rvest::html_text(rvest::html_node(linhas[1], "td"), trim = TRUE) %>% gsub("Nome: ", "", .)
        cpf_cnpj <- rvest::html_text(rvest::html_node(linhas[2], "td"), trim = TRUE) %>% gsub("CPF/CNPJ: ", "", .)
        tipo <- rvest::html_text(rvest::html_node(linhas[3], "td"), trim = TRUE) %>% gsub("Tipo: ", "", .)
        return(data.frame(codigo_car = codigo_car, id = id, nome = nome, cpf_cnpj = cpf_cnpj, tipo = tipo, stringsAsFactors = FALSE))
      } else {
        return(data.frame(codigo_car = codigo_car, id = id, nome = NA, cpf_cnpj = NA, tipo = NA, stringsAsFactors = FALSE))
      }
    })

    # Combinar os resultados em um único dataframe
    return(do.call(rbind, result_list))
  }

  # Criar uma barra de progresso, se fornecida
  if (!is.null(pb)) pb <- progress::progress_bar$new(total = nrow(resultado_car))

  # Aplicar a função para cada imóvel
  resultado <- purrr::map2_df(resultado_car$codigo, resultado_car$id, processar_dominio)

  return(resultado)
}
