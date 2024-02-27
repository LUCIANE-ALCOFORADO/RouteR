#' Gera a rota de menor distância
#'
#' @author Luciane Ferreira Alcoforado



#Dados<- read.csv2("C:/Users/TPC02/Documents/PesquisaOperacional/R_lpsolve/df_transposto.csv")
Dados = df_transposto[,]
#View(Dados) #verificando como estão os dados
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

tabela<-data.frame(var=colnames(Dados)[2:(n+1)],solucao = solucao.problema$solution)
tabela
require(dplyr)
tabela_rota<- tabela %>% filter(solucao==1)
tabela_rota


# Escrever o dataframe em um arquivo CSV
write.csv2(df_transposto, "df_transposto.csv", row.names = FALSE)
