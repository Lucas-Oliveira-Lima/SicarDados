#' Função para extrair dados detalhados de um imóvel no SICAR a partir do ID
#'
#' @param id ID do imóvel obtido da função `sicar_search_car_code`.
#' @return Dataframe com os dados extraídos dos links relacionados ao imóvel.
#' @export
extrair_dados_link <- function(id) {
  # URL base para os links
  url_base <- "http://car.semas.pa.gov.br/site/imovel/"
  
  # Links específicos (atualmente apenas domínio, mas pode ser expandido)
  links <- list(
    dominio = paste0(url_base, id, "/dominioFichaResumida")
  )
  
  # Dataframe para armazenar todos os dados
  df_dados <- data.frame()
  
  # Criar a barra de progresso
  pb <- progress::progress_bar$new(
    total = length(links),
    format = "  Extraindo dados [:bar] :percent em :elapsed",
    clear = FALSE
  )
  
  # Iterar sobre os links
  for (nome_link in names(links)) {
    url <- links[[nome_link]]
    
    # Extrair dados do link e armazenar no dataframe
    df_temp <- sicar_get_dominialidade(url, nome_link)
    
    # Combinar os dados temporários com o dataframe principal
    if (nrow(df_temp) > 0) {
      df_dados <- dplyr::bind_rows(df_dados, df_temp)
    }
    
    # Atualizar a barra de progresso
    pb$tick()
  }
  
  return(df_dados)
}

#' Função auxiliar para extrair dados de uma tabela HTML a partir de uma URL
#'
#' @param url URL da página onde os dados estão localizados.
#' @param nome_link Nome do link para ser usado como prefixo nas colunas do dataframe.
#' @return Dataframe com os dados extraídos da tabela HTML.
#' @export
sicar_get_dominialidade <- function(url, nome_link) {
  # Fazer a requisição GET
  resposta <- httr::GET(url)
  
  # Verificar se a requisição foi bem-sucedida
  if (httr::http_error(resposta)) {
    warning(paste("Erro ao acessar URL:", url))
    return(data.frame()) # Retorna um dataframe vazio em caso de erro
  }
  
  # Extrair conteúdo HTML
  conteudo_html <- httr::content(resposta, "text", encoding = "UTF-8")
  
  # Parsear HTML
  html <- rvest::read_html(conteudo_html)
  
  # Inicializar lista para armazenar dados da tabela
  lista_dados <- list()
  
  # Extrair dados da tabela HTML
  tables <- rvest::html_nodes(html, "table")
  for (table in tables) {
    # Extrair linhas da tabela
    rows <- rvest::html_nodes(table, "tr")
    for (row in rows) {
      # Extrair células da linha
      cells <- rvest::html_nodes(row, "td")
      if (length(cells) > 0) {
        # Extrair texto das células
        row_data <- rvest::html_text(cells, trim = TRUE)
        lista_dados <- c(lista_dados, row_data)
      }
    }
  }
  
  # Transformar a lista em um dataframe
  if (length(lista_dados) > 0) {
    df <- data.frame(t(lista_dados), stringsAsFactors = FALSE)
    colnames(df) <- paste0(nome_link, "_", seq_along(df))
  } else {
    df <- data.frame() # Retorna um dataframe vazio se nenhum dado for encontrado
  }
  
  return(df)
}
