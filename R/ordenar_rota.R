#' @title Função para ordenar os pontos de partida
#' @description
#' Organiza a solução do modelo por tipo de veículo e rota, explicitando o início e fim de cada rota.
#'
#'
#' @param rotas_tipo Dataframe com duas colunas: uma contendo a rota e outra contendo o tipo de veículo
#'
#' @return Dataframe com o tipo de veículo, a rota, a origem e o destino de cada rota
#' @export
#'

ordenar_rota <- function(rotas_tipo) {
  # Ordenar os pontos de partida com base na distância entre o segundo número de uma linha e o primeiro número da linha seguinte
  rotas_tipo<- rotas_tipo %>% dplyr::mutate(prod=as.numeric(substr(rotas_tipo$Rota, 1, 1))*as.numeric(substr(rotas_tipo$Rota, 2, 2)),origem=as.numeric(substr(rotas_tipo$Rota, 1, 1)),destino=as.numeric(substr(rotas_tipo$Rota, 2, 2))) %>%
    dplyr::arrange(Veiculo,prod, origem) %>% dplyr::select(Veiculo,Rota,origem,destino)
  #rotas_tipo <- rotas_tipo[order(c(NA, abs(rotas_tipo$Rota[-1] - rotas_tipo$Veiculo[-nrow(rotas_tipo)]))), ]

  return(rotas_tipo)
}

#' @examples
#' vetor <- c("x121", "x122", "x125", "x211", "x212", "x213", "x311", "x312", "x313", "x135")
#' obter_rotas(vetor)
#' ordenar_rota(obter_rotas(vetor))
