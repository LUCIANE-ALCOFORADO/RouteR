#' Gera a rota de menor distância
#'
#' @author Luciane Ferreira Alcoforado
#'
#' @description
#' Fornece uma lista contendo a solução do problema caso exista.
#'
#' @param Dados dataframe contendo uma coluna denominada cod com os códigos de cada linha, seguido das colunas das variáveis, depois coluna direcao contendo as desigualdades de cada restrição e a última coluna b contendo os limites de cada restrição
#'
#' @return uma lista de 3 objetos: $rota um dataframe com a rota, isto é, as variáveis com solução 1; $solução vetor contendo a solução de cada variável do modelo e $objetivo contendo o valor da função objetivo, ou seja a distância mínima percorrida pelos veículos.
#' @export
#'
#' @examples
#' n=5
#' k=1
#' vetor_custos=c(2.8,2,100,100,
#'+ 2.8,2.4,4.4,5.2,
#'+ 2,2.1,4.2,5,
#'+ 100,3.6,4.1,0.65,
#'+ 0,0,0,0)
#' custo <- gerar_custo(n,n,k,vetor_custos)
#' print(custo)
#' n_restricao<-1
#' saida <- gerar_restricoes_saida_origem(n,n,k,n_restricao)
#' saida
#' n_restricao<- nrow(saida)+1
#' n_restricao
#' retorno <- gerar_restricoes_retorno_origem(n,n,k,n_restricao)
#' retorno
#' n_restricao<- n_restricao+nrow(retorno)
#' n_restricao
#' equilibrio <- gerar_restricoes_equilibrio(n,n,k,n_restricao)
#' equilibrio
#' n_restricao<- n_restricao+nrow(equilibrio)
#' n_restricao
#' unicidade <- gerar_restricoes_unicidade(n,n,k,n_restricao)
#' unicidade
#' n_restricao<- n_restricao+nrow(unicidade)
#' n_restricao
#' subrota1 <- gerar_restricoes_subrota(n,n,k,n_restricao,2)
#' subrota1
#' #Para unir várias restrições utilizar
#' Dados <- dplyr::bind_rows(custo,saida,retorno,equilibrio,unicidade,subrota1)
#' gerar_rota(Dados)
#' #Ex2 considerando incluir subrota de tamanho 2
#' subrota2 <- gerar_restricoes_subrota(n,n,k,n_restricao,3)
#' subrota2
#' Dados2 <- dplyr::bind_rows(Dados, subrota2)
#' gerar_rota(Dados2)
#' vetor_rota<-gerar_rota(Dados2)$rota$var
#' #Organizando a rota obtida
#' organizar_rota(vetor_rota)
#' ordenar_rota(vetor_rota)
#' #Ex3 gerando distâncias aleatórias no custo
#' n=5
#' k=2
#' custo <- gerar_custo(n,n,k)
#' print(custo)
#' n_restricao<-1
#' saida <- gerar_restricoes_saida_origem(n,n,k,n_restricao)
#' saida
#' n_restricao<- nrow(saida)+1
#' n_restricao
#' retorno <- gerar_restricoes_retorno_origem(n,n,k,n_restricao)
#' retorno
#' n_restricao<- n_restricao+nrow(retorno)
#' n_restricao
#' equilibrio <- gerar_restricoes_equilibrio(n,n,k,n_restricao)
#' equilibrio
#' n_restricao<- n_restricao+nrow(equilibrio)
#' n_restricao
#' unicidade <- gerar_restricoes_unicidade(n,n,k,n_restricao)
#' unicidade
#' n_restricao<- n_restricao+nrow(unicidade)
#' n_restricao
#' subrota1 <- gerar_restricoes_subrota(n,n,k,n_restricao,2)
#' subrota1
#' #Para unir várias restrições utilizar
#' Dados3 <- dplyr::bind_rows(custo,saida,retorno,equilibrio,unicidade,subrota1)
#' gerar_rota(Dados3)
#' vetor_rota<-gerar_rota(Dados3)$rota$var
#' #Organizando a rota obtida
#' organizar_rota(vetor_rota)
#' ordenar_rota(vetor_rota)
gerar_rota<-function(Dados){
  solucao=NULL #truque para passar no teste
  if(ncol(Dados)<4|nrow(Dados)<2){
    warning("Dados deve ter coluna cod, x_{ijk}, direcao e b nesta ordem, com i,j e k variando de acordo com o problema")
  }
#n = n. variaveis e m = número restrições
n = ncol(Dados)-3 #descontar 3 devido coluna cod, direcao e b
m = nrow(Dados)-1 #descontar 1 devido a linha custo

coef.restricoes = as.matrix(Dados[2:(m+1),2:(n+1)])
direcao.restricoes = Dados$direcao[2:(m+1)]
limites.restricoes = Dados$b[2:(m+1)]
func.objetivo = as.vector(t(Dados[(1),2:(n+1)]))
solucao.problema =
  lpSolve::lp(direction = "min",
             objective.in = func.objetivo,
             const.mat = coef.restricoes,
             const.dir = direcao.restricoes,
             const.rhs = limites.restricoes,
             all.int=T)
# valor da função objetivo
solucao.problema$objval
# Valores para as variáveis de decisão
solucao.problema$solution

tabela_solucao<-data.frame(var=colnames(Dados)[2:(n+1)],solucao = solucao.problema$solution)
tabela_solucao

tabela_rota<- tabela_solucao %>% dplyr::filter(solucao==1)
tabela_rota

return(list(rota=tabela_rota,solucao=solucao.problema$solution,objetivo=solucao.problema$objval))
}



