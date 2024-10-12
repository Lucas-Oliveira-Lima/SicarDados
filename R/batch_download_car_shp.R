#' Função para baixar shapefiles de vários imóveis CAR
#'
#' @param df Dataframe contendo os códigos CAR
#' @param coluna_codigo Nome da coluna que contém os códigos CAR
#' @param diretorio_destino Diretório onde os shapefiles serão salvos (opcional). Se não informado, será criado 'data/shapefile'.
#' @export
batch_download_car_shp <- function(df, coluna_codigo, diretorio_destino = NULL) {

  # Definir o diretório de destino padrão se não informado
  if (is.null(diretorio_destino)) {
    diretorio_destino <- file.path(getwd(), "data", "shapefile")
  }

  # Loop sobre cada linha do dataframe
  for (i in 1:nrow(df)) {
    codigo_imovel <- df[[coluna_codigo]][i]  # Obter o código CAR de cada linha

    # Chamar a função para baixar o imóvel
    sicar_download_car_shp(codigo_imovel, diretorio_destino)

    # Pausar 1 segundo entre os downloads para evitar sobrecarga no servidor
    # Sys.sleep(1)
  }
}
