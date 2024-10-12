#' Função para consultar a dominialidade de vários imóveis CAR
#'
#' @param codigos_car Vetor de códigos CAR para consulta
#' @param pb (opcional) Barra de progresso da consulta.
#' @return Um dataframe consolidado com as informações de dominialidade (nome, CPF/CNPJ, tipo, código CAR e ID).
#' @export
consultar_dominialidade_varios_car <- function(codigos_car, pb = NULL) {

  # Verificar se os códigos estão em um vetor
  if (!is.vector(codigos_car)) {
    stop("Os códigos CAR devem ser fornecidos em um vetor.")
  }

  # Função auxiliar para cada código CAR
  consultar_um_car <- function(codigo_car) {
    consultar_dominialidade_car(codigo_car, pb)
  }

  # Criar uma barra de progresso, se fornecida
  if (!is.null(pb)) pb <- progress::progress_bar$new(total = length(codigos_car))

  # Aplicar a função para cada código CAR
  resultado <- purrr::map_df(codigos_car, consultar_um_car)

  return(resultado)
}
