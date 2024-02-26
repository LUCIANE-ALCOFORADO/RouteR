#' Gera restrições de saída da origem
#'
#'@description
#'Gera as restrições de saída da origem do modelo, ou seja, todo veículo deve sair do ponto 1 (origem).
#'
#' @param i_limit número de nós do problema que representam a origem num trecho da rota
#' @param j_limit número de nós do problema que representam o destino num trecho da rota
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

gerar_restricoes_saida_origem <- function(i_limit, j_limit, k_limit, n_restricao) {
   if (i_limit != j_limit){warning("Os valores de i_limit e j_limit devem ser iguais, será considerado i_limit para todos os efeitos")
  j_limit <- i_limit}
  if (i_limit <= 1){warning("Valor de i_limit deve ser maior do que 1, será considerado igual a 2")
    i_limit <- 2
    j_limit <- i_limit}
    restricoes <- matrix(0, ncol = (i_limit * j_limit*k_limit - i_limit*k_limit) , nrow = k_limit)
#restricoes <- matrix(0)
    Res <- integer()
for (k1 in 1:k_limit) {

  for (i in 1:i_limit) {
      for (j in 1:j_limit) {
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

  for (i in 1:i_limit) {
    for (j in 1:j_limit) {
      if (i != j) {

         for (k in 1:k_limit) {
            vetor_caracteres <- c(vetor_caracteres, paste0("x", i, j, k))

          }
      }
    }
  }


  restricoes<-data.frame(cod=1:dim(restricoes)[1],restricoes, direcao=rep("=",dim(restricoes)[1]),b=rep(1,dim(restricoes)[1]))
  rownames(restricoes) <- paste0("R_", n_restricao:(k_limit+n_restricao-1))
  colnames(restricoes)<- c("cod",vetor_caracteres,"direcao","b")
  return(restricoes)
}


