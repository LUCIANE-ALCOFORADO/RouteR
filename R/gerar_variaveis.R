#' Gera nomes das variáveis
#'
#' @author Luciane Ferreira Alcoforado
#'
#' @description
#' Gera um vetor contendo os nomes das variáveis x_{ijk}. Na modelagem x_{ijk} é uma variável binária igual a 1 se o trecho (ij) for incluído na rota com o veículo k; e igual a zero se o trecho não for incluido na rota com o veículo k.
#'
#'
#' @param i_limit Número inteiro que representa a quantidade de pontos (cidades) de partida no modelo, incluindo a origem.
#' @param j_limit Número inteiro que representa a quantidade de pontos (cidades) de chegada no modelo, incluindo a origem como destino final.
#' @param k_limit Número inteiro que representa a quantidade de veículos que partem da origem.
#'
#'
#' @return vetor caracter contendo os nomes das variáveis do modelo
#'@export
#'
#' @examples
#' gerar_variaveis(3,3)
#' gerar_variaveis(4,4,2)
#'
gerar_variaveis<-function(i_limit, j_limit, k_limit=1){
  if (i_limit != j_limit){warning("Os valores de i_limit e j_limit devem ser iguais, será considerado i_limit para todos os efeitos")
  j_limit <- i_limit}
  if (i_limit <= 1){warning("Valor de i_limit deve ser maior do que 1, será considerado igual a 2")
    i_limit <- 2
    j_limit <- i_limit}
  indices<-tidyr::expand_grid(i = 1:i_limit, j = 1:j_limit, k=1:k_limit)
  indices<-indices %>% dplyr::filter(i!=j) %>% dplyr::mutate(x=paste0("x",i,j,k))
  return(indices)}
