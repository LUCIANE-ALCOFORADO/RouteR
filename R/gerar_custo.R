#'#Funções para gerar planilha de dados para o
#'#Problema de roteamento de k veículos, k=1, ..., disponibilidade de veículos.
#'#Considerando uma origem, n clientes e retorna para origem.

#'#O usuário deverá informar quantos clientes, quantos veículos
#'@title Produz os coeficientes do vetor custo do modelo de otimização linear de roteamento de veículos
#'@name gerar_custo
#'@author Luciane Ferreira Alcoforado
#'@description Produz um vetor contendo os custos (distância entre dois nós) para o modelo matemático

#'@param i_limit Número inteiro que representa a quantidade de pontos (cidades) de partida no modelo, incluindo a origem.
#'@param j_limit Número inteiro que representa a quantidade de pontos (cidades) de chegada no modelo, incluindo a origem como destino final.
#'@param k_limit Número inteiro que representa a quantidade de veículos que partem da origem.
#'@param vetor_custos  Vetor de números reais representando a distância entre o trecho (ij) no veículo k, deve seguir a ordem estabelecida na sequencia de variáveis do modelo. Caso não seja fornecido, a função gera valores aleatórios (runif) e simétricos, ou seja distância entre i e j é a mesma entre j e i.
#'@return Dataframe contendo os coeficientes da função objetivo do modelo, ou seja, as distâncias entre cada trecho (ij) no veículo k.
#'@import tidyr
#'@importFrom("stats", "runif")

#'
#'@examples
#'# Exemplo de uso da função sem vetor_custos (usando runif para gerar custos)
#'resultado1 <- gerar_custo(5, 5, 3)
#'print(resultado1)

#'# Exemplo de uso da função com vetor_custos fornecido pelo usuário
#'vetor_custos_usuario <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
#'resultado2 <- gerar_custo(7, 10, 3, vetor_custos_usuario)
#'print(resultado2)


#'# Exemplo de uso da função com vetor_custos fornecido pelo usuário
#'vetor_custos_usuario <- c(10, 20, 30, 40, 50, 60)
#'resultado3 <- gerar_custo(2, 2, 3, vetor_custos_usuario)
#'print(resultado3)
#'
#'n=5
#'k=1
#'custo <- gerar_custo(n,n,k,
#'vetor_custos=c(2.8,2,100,100,
#'2.8,2.4,4.4,5.2,
#'2,2.1,4.2,5,
#'100,3.6,4.1,0.65,
#'0,0,0,0))
#'print(custo)
#'custo2 <- gerar_custo(-1, 5, 3)
#'print(custo2)
#'@export
gerar_custo <- function(i_limit, j_limit, k_limit, vetor_custos = NULL) {
   if (i_limit != j_limit){warning("Os valores de i_limit e j_limit devem ser iguais, será considerado i_limit para todos os efeitos")
  j_limit <- i_limit}
  if (i_limit <= 1){warning("Valor de i_limit deve ser maior do que 1, será considerado igual a 2")
    i_limit <- 2
    j_limit <- i_limit}
   # Verificar se o tamanho do vetor_custos é válido
  if (!is.null(vetor_custos)) {
    expected_length <- i_limit^2 * k_limit - k_limit * i_limit
    if (length(vetor_custos) != expected_length) {
      stop("O comprimento do vetor_custos não é válido.")
    }
  }

  vetor_caracteres <- character()
  vetor_custos <- numeric()
if (is.null(vetor_custos) || length(vetor_custos) == 0) {
  for (i in 1:i_limit) {
    for (j in 1:j_limit) {
      if (i != j) {

          if (i < j) {
            set.seed(i+j)
            custo_ij <- runif(1, 1, 100)
          } else {
            index <- match(paste0("x", j, i, 1), vetor_caracteres)
            custo_ij <- vetor_custos[index]
          }

          for (k in 1:k_limit) {
            vetor_caracteres <- c(vetor_caracteres, paste0("x", i, j, k))
            vetor_custos <- c(vetor_custos, custo_ij)
          }

      }
    }
  }
} else {
          for (i in 1:i_limit) {
            for (j in 1:j_limit) {
              if (i != j) {
                for (k in 1:k_limit) {
            vetor_caracteres <- c(vetor_caracteres, paste0("x", i, j, k))
                }
              }
            }
          }
          vetor_custos <- vetor_custos
}
  dados <- data.frame(vetor_caracteres, vetor_custos)
  novo_dataframe <- tidyr::pivot_wider(dados, names_from = vetor_caracteres, values_from = vetor_custos)
novo_dataframe<-data.frame(cod=1:dim(novo_dataframe)[1],novo_dataframe, direcao=rep("=",dim(novo_dataframe)[1]),b=rep(1,dim(novo_dataframe)[1]))
 rownames(novo_dataframe) <- "custo"
  return(novo_dataframe)
}


