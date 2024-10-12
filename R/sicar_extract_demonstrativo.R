#' Função para extrair o texto de uma div com base no id e na classe
#'
#' @param page HTML da página do imóvel
#' @param div_id ID da div que contém o dado desejado
#' @param class Classe do HTML onde os dados estão
#' @return Texto extraído ou NA caso o dado não seja encontrado
#' @export
extrair_dados_demonstrativo <- function(page, div_id, class){
  result <- page %>%
    rvest::html_node(paste0("#", div_id)) %>%
    rvest::html_nodes(paste0(".", class)) %>%
    rvest::html_text(trim = TRUE)

  if(length(result) == 0) {
    return(NA)  # Retorna NA se o resultado estiver vazio
  } else {
    return(result)
  }
}

#' Função para processar um imóvel pelo código CAR e extrair dados do demonstrativo
#'
#' @param car_code Código CAR do imóvel a ser processado
#' @return Dataframe com os dados extraídos do imóvel
#' @export
sicar_extract_demonstrativo <- function(car_code) {
  url <- paste0("http://car.semas.pa.gov.br/site/demonstrativo/imovel/", car_code)
  pagina <- tryCatch(rvest::read_html(url), error = function(e) return(NA))

  if (is.na(pagina)) {
    return(data.frame(car_code = car_code, situacao_imovel = NA, numero_recibo = NA, municipio = NA,
                      area_ha = NA, modulos_fiscais = NA, centroides = NA, nome_cadastrante = NA,
                      cpf_cadastrante = NA, art_cadastrante = NA, area_antropizada_nao_consolidada = NA,
                      area_consolidada = NA, area_rvn = NA, area_regeneracao_total = NA,
                      area_reserva_legal = NA, area_app = NA, area_uso_alternativo_solo = NA,
                      area_rl_regularizar = NA, area_app_recompor = NA, area_uso_restrito_recompor = NA,
                      stringsAsFactors = FALSE))
  }

  # Extrair os dados de interesse
  situacao_imovel <- extrair_dados_demonstrativo(pagina, "titulo-dados-imovel", "col-md-3")
  numero_recibo <- extrair_dados_demonstrativo(pagina, "numero-recibo", "col-md-8")
  municipio <- extrair_dados_demonstrativo(pagina, "municipio", "col-md-8")
  area_ha <- extrair_dados_demonstrativo(pagina, "area-imovel", "col-md-8")
  modulos_fiscais <- extrair_dados_demonstrativo(pagina, "modulos-fiscais", "col-md-8")
  centroides <- extrair_dados_demonstrativo(pagina, "centroide", "col-md-8")

  nome_cadastrante <- extrair_dados_demonstrativo(pagina, "nome-cadastrante", "col-md-9")
  cpf_cadastrante <- extrair_dados_demonstrativo(pagina, "art-cadastrante", "col-md-9")
  art_cadastrante <- extrair_dados_demonstrativo(pagina, "registro-cadastrante", "col-md-9")

  area_antropizada_nao_consolidada <- extrair_dados_demonstrativo(pagina, "area-antropizada", "col-md-3")
  area_consolidada <- extrair_dados_demonstrativo(pagina, "area-consolidada", "col-md-3")
  area_rvn <- extrair_dados_demonstrativo(pagina, "area-rvn", "col-md-3")
  area_regeneracao_total <- extrair_dados_demonstrativo(pagina, "area-regeneracao", "col-md-3")

  area_reserva_legal <- extrair_dados_demonstrativo(pagina, "dados-reserva-legal", "col-md-3")
  area_app <- extrair_dados_demonstrativo(pagina, "dados-APP", "col-md-3")

  area_uso_alternativo_solo <- extrair_dados_demonstrativo(pagina, "uso-alternativo", "col-md-2")
  area_rl_regularizar <- extrair_dados_demonstrativo(pagina, "area-rl-regularizar", "col-md-2")
  area_app_recompor <- extrair_dados_demonstrativo(pagina, "area-app-recompor", "col-md-2")
  area_uso_restrito_recompor <- extrair_dados_demonstrativo(pagina, "area-ur-recompor", "col-md-2")

  # Criando um dataframe para este imóvel
  df <- data.frame(
    car_code = car_code,
    situacao_imovel = situacao_imovel,
    numero_recibo = numero_recibo,
    municipio = municipio,
    area_ha = area_ha,
    modulos_fiscais = modulos_fiscais,
    centroides = centroides,
    nome_cadastrante = nome_cadastrante,
    cpf_cadastrante = cpf_cadastrante,
    art_cadastrante = art_cadastrante,
    area_antropizada_nao_consolidada = area_antropizada_nao_consolidada,
    area_consolidada = area_consolidada,
    area_rvn = area_rvn,
    area_regeneracao_total = area_regeneracao_total,
    area_reserva_legal = area_reserva_legal,
    area_app = area_app,
    area_uso_alternativo_solo = area_uso_alternativo_solo,
    area_rl_regularizar = area_rl_regularizar,
    area_app_recompor = area_app_recompor,
    area_uso_restrito_recompor = area_uso_restrito_recompor,
    stringsAsFactors = FALSE
  )

  # Pós-processamento: Substituir strings específicas
  df$situacao_imovel <- gsub("Situação:     ", "", df$situacao_imovel)
  colunas_area <- c("area_ha", "area_antropizada_nao_consolidada", "area_consolidada", "area_rvn",
                    "area_regeneracao_total", "area_reserva_legal", "area_app", "area_uso_alternativo_solo",
                    "area_rl_regularizar", "area_app_recompor", "area_uso_restrito_recompor")

  df[colunas_area] <- lapply(df[colunas_area], function(x) as.numeric(gsub(" ha", "", x)))

  return(df)
}

