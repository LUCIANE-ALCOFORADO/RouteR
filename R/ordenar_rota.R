#' @title Função para ordenar os pontos de partida
#'
#' @author Luciane Ferreira Alcoforado
#'
#' @description
#' Organiza a solução do modelo por tipo de veículo e rota, explicitando o início e fim de cada rota.
#'
#'
#' @param vetor vetor contendo as variáveis xijk do modelo
#'
#' @return Dataframe com o tipo de veículo, a rota, a origem e o destino de cada rota
#' @export
#' @examples
#' vetor <- c("x121", "x142", "x175", "x231", "x452", "x613", "x311", "x512", "x163", "x715")
#' organizar_rota(vetor)
#' ordenar_rota(vetor)
#'
#' vetor<-c("x_1,3,1", "x_2,4,1", "x_3,2,1", "x_4,5,1", "x_5,1,1")
#' ordenar_rota(vetor)
ordenar_rota <- function(vetor) {
   # dummies to trick R CMD check
  Veiculo <- NULL; origem <- NULL; Trecho <- NULL; destino <- NULL
df<- organizar_rota(vetor)
  # Ordenar os pontos de partida com base na distância entre o segundo número de uma linha e o primeiro número da linha seguinte
  df<- df %>% dplyr::arrange(Veiculo,origem)
rota_tipo=NULL


  for (k in seq_along(unique(df$Veiculo)) ){
    # Inicialize as variáveis
    df_k <- df %>% dplyr::filter(Veiculo==unique(df$Veiculo)[k])
    n <- nrow(df_k)
  nova_origem <- numeric(n)
  novo_destino <- numeric(n)
  novo_trecho <- numeric(n)
  nova_origem[1] <- df$origem[1]
  novo_destino[1] <- df$destino[1]
  novo_trecho[1] <- df$Trecho[1]
  for (i in 2:n) {
    origem_anterior <- novo_destino[i - 1]
    indice_origem_anterior <- which(df$origem == origem_anterior)
    nova_origem[i] <- df$origem[indice_origem_anterior]
    novo_destino[i] <- df$destino[indice_origem_anterior]
    novo_trecho[i] <- paste0(nova_origem[i],"-",novo_destino[i])
  }
  df_ord<- dplyr::bind_rows(Veiculo=rep(unique(df$Veiculo)[k],nrow(df %>% dplyr::filter(Veiculo==unique(df$Veiculo)[k]))),Trecho=novo_trecho, origem=nova_origem, destino=novo_destino)
  rota_tipo<-dplyr::bind_rows(rota_tipo,df_ord)
  }

  return(rota_tipo)
}


