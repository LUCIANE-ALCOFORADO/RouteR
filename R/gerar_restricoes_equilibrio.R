#' Gera Restrições de Equilíbrio
#'
#' @author Luciane Ferreira Alcoforado
#' @description
#'Gera as restrições de equilíbrio do modelo, ou seja, todo veículo que chega em um ponto de passagem da rota deve partir para outro ponto.
#'
#' @param i_limit número de nós do problema que representam a origem num trecho da rota
#' @param j_limit número de nós do problema que representam o destino num trecho da rota
#' @param k_limit número de veículos do problema
#' @param n_restricao número de controle da restrição, por padrão terá ser valor igual a 1, podendo ser renumerado de acordo com a modelagem adotada.
#'
#' @return retorna um vetor contendo as restrições de equilíbrio.
#' @export
#'
#' @examples
#' # Exemplo de uso da função
#' restricoes_equilibrio <- gerar_restricoes_equilibrio(5,5,3,4)
#' print(restricoes_equilibrio)
#'
#' restricoes_equilibrio1 <- gerar_restricoes_equilibrio(5,5,3)
#' print(restricoes_equilibrio1)
#'
#' restricoes_equilibrio2 <- gerar_restricoes_equilibrio(1,5,3)
#' print(restricoes_equilibrio2)
#' restricoes_equilibrio3 <- gerar_restricoes_equilibrio(5,1,3)
#' print(restricoes_equilibrio3)
#'
gerar_restricoes_equilibrio<-function(i_limit, j_limit, k_limit, n_restricao=1) {
   if (i_limit != j_limit){warning("Os valores de i_limit e j_limit devem ser iguais, será considerado i_limit para todos os efeitos")
  j_limit <- i_limit}
  if (i_limit <= 1){warning("Valor de i_limit deve ser maior do que 1, será considerado igual a 2")
    i_limit <- 2
    j_limit <- i_limit}
    restricoes <- matrix(0, ncol = (i_limit * j_limit*k_limit - i_limit*k_limit) , nrow = k_limit*(i_limit-1))
#restricoes <- matrix(0)
    Res <- integer()
    cont=1
    for (k1 in 1:k_limit) {
      for (i1 in 2:i_limit) {
        for (i in 1:i_limit) {
          for (j in 1:j_limit) {
            if (i != j) {  # Garantir que i e j sejam diferentes
      # Gerar Restrições de equilibrio
              for (k in 1:k_limit) {
        valor_equilibrio <- ifelse(k == k1&i==i1, 1, ifelse(k==k1&j==i1,-1,0))
        Res <- c(Res,valor_equilibrio)
              }
            }
         }
      }
        restricoes[cont,] <- Res
        Res<- integer()
        cont<-cont+1
      }
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


  restricoes<-data.frame(cod=1:dim(restricoes)[1],restricoes, direcao=rep("=",dim(restricoes)[1]),b=rep(0,dim(restricoes)[1]))
  rownames(restricoes) <- paste0("R_", n_restricao:(n_restricao+dim(restricoes)[1]-1))
  colnames(restricoes)<- c("cod",vetor_caracteres,"direcao","b")
  return(restricoes)
}
