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
#' vetor<-c("x131", "x241", "x321", "x451", "x511")
#' ordenar_rota(vetor)
ordenar_rota <- function(vetor) {
   # dummies to trick R CMD check
  Veiculo <- NULL; origem <- NULL; Rota <- NULL; destino <- NULL
df<- organizar_rota(vetor)
  # Ordenar os pontos de partida com base na distância entre o segundo número de uma linha e o primeiro número da linha seguinte
  df<- df %>% dplyr::mutate(origem=as.numeric(substr(df$Rota, 1, 1)),destino=as.numeric(substr(df$Rota, 2, 2))) %>%
    dplyr::arrange(Veiculo,origem) %>% dplyr::select(Veiculo,Rota,origem,destino)
  #rotas_tipo <- rotas_tipo[order(c(NA, abs(rotas_tipo$Rota[-1] - rotas_tipo$Veiculo[-nrow(rotas_tipo)]))), ]

  rota_tipo=NULL


  for (k in seq_along(unique(df$Veiculo)) ){
    # Inicialize as variáveis
    df_k <- df %>% dplyr::filter(Veiculo==unique(df$Veiculo)[k])
    n <- nrow(df_k)
  nova_origem <- numeric(n)
  novo_destino <- numeric(n)
  nova_rota <- numeric(n)
  nova_origem[1] <- df$origem[1]
  novo_destino[1] <- df$destino[1]
  nova_rota[1] <- df$Rota[1]
  for (i in 2:n) {
    origem_anterior <- novo_destino[i - 1]
    indice_origem_anterior <- which(df$origem == origem_anterior)
    nova_origem[i] <- df$origem[indice_origem_anterior]
    novo_destino[i] <- df$destino[indice_origem_anterior]
    nova_rota[i] <- paste0(nova_origem[i],novo_destino[i])
  }
  df_ord<- dplyr::bind_rows(Veiculo=rep(unique(df$Veiculo)[k],nrow(df %>% dplyr::filter(Veiculo==unique(df$Veiculo)[k]))),Rota=nova_rota, origem=nova_origem, destino=novo_destino)
  rota_tipo<-dplyr::bind_rows(rota_tipo,df_ord)
  }

  return(rota_tipo)
}


