#' Organiza a rota para cada veículo
#'
#'@author Luciane Ferreira Alcoforado
#'
#' @description
#' Organizar a rota para cada veiculo com base em uma solução
#'
#'
#'
#' @param vetor nomes das variáveis de valor 1 na solução do modelo (solução obtida utilizando por exemplo o pacote`lpSolve`)
#'
#' @return Dataframe contendo os índices ij de partida e chegada e o código do veículo correspondente à rota.
#' @examples
#'
#' # Exemplo de uso da função
#' vetor <- c("x_1,2,1", "x_1,2,2", "x_1,2,5", "x_2,1,1", "x_2,1,2", "x_2,1,3", "x_3,1,1", "x_3,1,2", "x_3,1,3", "x_1,3,5")
#' rotas_por_tipo <- organizar_rota(vetor)
#'
#' @export
organizar_rota <- function(vetor) {
  # Extrair os números usando expressões regulares
 df <- data.frame(vetor) %>%
   dplyr::mutate(
     n1 = as.numeric(stringr::str_extract(vetor, "(?<=x_)\\d+")),
     n2 = as.numeric(stringr::str_extract(vetor, "(?<=,)\\d+(?=,)")),
     n3 = as.numeric(stringr::str_extract(vetor, "(?<=,)\\d+$"))
   ) %>%
   dplyr::select(-vetor) %>% dplyr::arrange(n3,n1,dplyr::desc(n2))%>% dplyr::mutate(trecho=paste0(df$n1,"-",df$n2))
tabela<-data.frame(Veiculo=df$n3, Trecho=df$trecho, origem=df$n1, destino=df$n2)
  return(tabela)
}





