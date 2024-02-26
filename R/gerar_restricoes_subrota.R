#' Gera restrições de subrota
#' @description
#' Gera restrições de subrota de tamanho n-1, em que n representa o número de pontos na sub rota e deve variar de 2 até i_limit. Por padrão n = 2.
#'
#' @param i_limit número de nós do problema que representam a origem num trecho da rota
#' @param j_limit número de nós do problema que representam o destino num trecho da rota
#' @param k_limit número de veículos do problema
#' @param n_restricao número de controle da restrição, por padrão terá ser valor igual a 1, podendo ser renumerada de acordo com a modelagem adotada.
#' @param n número de pontos na rota, ou seja, sub rota de tamanho 1 terá trecho com dois pontos, rota de tamanho 2 terá trecho com 3 pontos, ...Por padrão n=2, ou seja, são geradas restrições de sub rota de tamanho 1.
#'
#' @return retorna um dataframe com as restrições de sub rota de tamanho n-1
#' @export
#'
#' @examples
#' # Exemplo de uso da função para sub rotas de tamanho 2
#' restricoes_subrota2 <- gerar_restricoes_subrota(5,5,3,1,3)
#' print(restricoes_subrota2)
#' # Exemplo de uso da função para sub rotas de tamanho 1
#' restricoes_subrota1 <- gerar_restricoes_subrota(5,5,3,1)
#' print(restricoes_subrota1)
#' # Exemplo de uso da função para sub rotas de tamanho 3
#' restricoes_subrota3 <- gerar_restricoes_subrota(5,5,3,1,4)
#' print(restricoes_subrota3)
#' restricoes_subrota4 <- gerar_restricoes_subrota(6,5,1,1,4)
#' print(restricoes_subrota4)
gerar_restricoes_subrota <- function(i_limit, j_limit, k_limit, n_restricao, n=2) {
#n é o tamanho (número de trechos) da sub-rota
if (n>i_limit-2){return("insira n <= i_limit-2")}
if (i_limit != j_limit){warning("Os valores de i_limit e j_limit devem ser iguais, será considerado i_limit para todos os efeitos")
  j_limit <- i_limit}
if (i_limit <= 1){warning("Valor de i_limit deve ser maior do que 1, será considerado igual a 2")
    i_limit <- 2
    j_limit <- i_limit}
indices<-tidyr::expand_grid(i = 1:i_limit, j = 1:j_limit, k=1:k_limit)
indices<-indices %>% dplyr::filter(i!=j) %>% dplyr::mutate(x=paste0("x",i,j,k))
ij<-t(utils::combn(i_limit,n))
df<-tidyr::expand_grid(ij,k=1:k_limit)
indices_subrota<-data.frame(matrix(unlist(df), nrow = nrow(df), byrow = F))
restricoes <- matrix(0, ncol = (i_limit * j_limit*k_limit - i_limit*k_limit) , nrow = k_limit*(dim(combn(i_limit,n))[2]))
#restricoes <- matrix(0)

colunas<- dim(indices_subrota)[2]
l =(colunas-1)

    for (cont in 1:(dim(indices_subrota)[1])) {
      Res <- integer()
          for (i in 1:i_limit) {
            for (j in 1:j_limit) {
              if (i != j) {  # Garantir que i e j sejam diferentes
      # Gerar Restrições de sub rota tamanho n
                for (k in 1:k_limit) {
          valor <- ifelse(k == indices_subrota[cont,colunas]&(
          i  %in% indices_subrota[cont,1:l] &
          j  %in% indices_subrota[cont,1:l] ), 1,0)
          Res <- c(Res,valor)
                }
              }
           }
          }
      restricoes[cont,] <- Res
     }




#nomes das colunas
#no inicio do código criei a tabela indices
#outra forma seria fazer vetor_caracteres <- indices$x
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


  restricoes<-data.frame(cod=1:dim(restricoes)[1],restricoes, direcao=rep("<=",dim(restricoes)[1]),b=rep((colunas-2),dim(restricoes)[1]))
  rownames(restricoes) <- paste0("R_", n_restricao:(n_restricao+dim(restricoes)[1]-1))
  colnames(restricoes)<- c("cod",vetor_caracteres,"direcao","b")
  return(restricoes)
}





