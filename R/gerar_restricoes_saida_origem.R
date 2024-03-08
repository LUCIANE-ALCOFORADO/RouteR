#' Gera restrições de saída da origem
#'
#' @author Luciane Ferreira Alcoforado
#'
#'@description
#'Gera as restrições de saída da origem do modelo, ou seja, todo veículo deve sair do ponto 1 (origem).
#'
#' @param i_limit vetor com número de nós do problema que representam a origem num trecho da rota
#' @param j_limit vetor com número de nós do problema que representam o destino num trecho da rota
#' @param k_limit número de veículos do problema
#' @param n_restricao número de controle da restrição, por padrão terá ser valor igual a 1, podendo ser renumerado de acordo com a modelagem adotada.
#'
#' @return retorna um dataframe contendo as restrições de saída da origem.
#'
#' @examples
#' # Exemplo de uso da função
#'restricoes_saida <- gerar_restricoes_saida_origem(5,5,3,1)
#'print(restricoes_saida)
#'restricoes_saida1 <- gerar_restricoes_saida_origem(5,5,3,11)
#'print(restricoes_retorno1)
#'
#' @export

gerar_restricoes_saida_origem <- function(i_limit, j_limit, k_limit=1, n_restricao=1) {

if (length(i_limit) <= 1){warning("Tamanho de i_limit deve ser maior do que 1, será considerado 1:2")
    i_limit <- 1:2}
  if (length(j_limit) <= 1){warning("Tamanho de j_limit deve ser maior do que 1, será considerado 1:2")
    j_limit <- 1:2}
    restricoes <- matrix(0, ncol = (length(i_limit) * length(j_limit)*k_limit - min(length(i_limit), length(j_limit))*k_limit) , nrow = k_limit)
#restricoes <- matrix(0)
    Res <- integer()
for (k1 in 1:k_limit) {

for (i in seq_along(i_limit)) {
    for (j in seq_along(j_limit)) {
        if (i != j) {  # Garantir que i e j sejam diferentes
      # Gerar Restrições de chegada e saida

       for (k in 1:k_limit) {
        valor_saida <- ifelse(i == 1&k==k1, 1, 0)
        Res <- c(Res,valor_saida)
        }
      }
  }
}
        restricoes[k1,] <- Res
        Res<- integer()
}

#nomes das colunas
  vetor_caracteres <- character()
#Modifiquei essa parte
#  for (i in 1:i_limit) {
#    for (j in 1:j_limit) {
#     if (i != j) {
#         for (k in 1:k_limit) {
#           vetor_caracteres <- c(vetor_caracteres, paste0("x", i, j, k))
#          }  }    }  }

vetor_caracteres <-tidyr::expand_grid(i = i_limit, j = j_limit, k=1:k_limit)
  vetor_caracteres <-vetor_caracteres  %>% dplyr::filter(i!=j) %>% dplyr::mutate(x=paste0("x_",i,",",j,",",k)) #separa os indices dos vértices com ','
  restricoes<-data.frame(cod=1:dim(restricoes)[1],restricoes, direcao=rep("=",dim(restricoes)[1]),b=rep(1,dim(restricoes)[1]))
  rownames(restricoes) <- paste0("R_", n_restricao:(k_limit+n_restricao-1))
  colnames(restricoes)<- c("cod",vetor_caracteres$x,"direcao","b")
  return(restricoes)
}


