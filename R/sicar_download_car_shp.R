#' Função para baixar o shapefile de um imóvel CAR
#'
#' @param codigo_imovel Código CAR do imóvel a ser baixado
#' @param diretorio_destino Diretório onde o shapefile será salvo (opcional). Se não informado, será criado 'data/shapefile'.
#' @export
sicar_download_car_shp <- function(codigo_imovel, diretorio_destino = NULL) {

  # URL base
  base_url <- "http://car.semas.pa.gov.br/site/consulta/imoveis/baixarShapeFile/"

  # Definir o diretório de destino padrão se não informado
  if (is.null(diretorio_destino)) {
    diretorio_destino <- file.path(getwd(), "data", "shapefile")
  }

  # Verificar se o diretório de destino existe, se não, criar
  if (!dir.exists(diretorio_destino)) {
    dir.create(diretorio_destino, recursive = TRUE)
  }

  # Definir o nome do arquivo a ser salvo (adicionar extensão .zip)
  nome_arquivo <- paste0(codigo_imovel, ".zip")
  caminho_arquivo <- file.path(diretorio_destino, nome_arquivo)

  # Fazer a requisição e baixar o arquivo
  resposta <- httr::GET(url, httr::write_disk(caminho_arquivo, overwrite = TRUE))

  # Verificar se o download foi bem-sucedido
  if (httr::status_code(resposta) == 200) {
    message(paste("Download concluído com sucesso:", caminho_arquivo))
  } else {
    warning(paste("Falha ao baixar o arquivo para o imóvel:", codigo_imovel))
  }
}


