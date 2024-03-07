#' Gera nomes das variáveis
#'
#' @author Luciane Ferreira Alcoforado
#'
#' @description
#' Gera um vetor contendo os nomes das variáveis x_{i,j,k}. Na modelagem x_{i,j,k} é uma variável binária igual a 1 se o trecho (i,j) for incluído na rota com o veículo k; e igual a zero se o trecho não for incluido na rota com o veículo k.
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
#' gerar_variaveis(i=c(1,3:5),j=c(1,8:10),k=2)
#'
gerar_variaveis<-function(i_limit=1:2, j_limit=1:4, k_limit=1){
  i=NULL; j=NULL; k=NULL #truque teste
  if (length(i_limit) <= 1){warning("Tamanho de i_limit deve ser maior do que 1, será considerado 1:2")
    i_limit <- 1:2}
  if (length(j_limit) <= 1){warning("Tamanho de j_limit deve ser maior do que 1, será considerado 1:2")
    j_limit <- 1:2}
  indices<-tidyr::expand_grid(i = i_limit, j = j_limit, k=1:k_limit)
  indices<-indices %>% dplyr::filter(i!=j) %>% dplyr::mutate(x=paste0("x_",i,",",j,",",k)) #separa os indices dos vértices com ','
  return(indices)}
