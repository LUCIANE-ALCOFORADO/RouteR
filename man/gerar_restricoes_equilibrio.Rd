% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gerar_restricoes_equilibrio.R
\name{gerar_restricoes_equilibrio}
\alias{gerar_restricoes_equilibrio}
\title{Gera Restrições de Equilíbrio}
\usage{
gerar_restricoes_equilibrio(i_limit, j_limit, k_limit, n_restricao = 1)
}
\arguments{
\item{i_limit}{número de nós do problema que representam a origem num trecho da rota}

\item{j_limit}{número de nós do problema que representam o destino num trecho da rota}

\item{k_limit}{número de veículos do problema}

\item{n_restricao}{número de controle da restrição, por padrão terá ser valor igual a 1, podendo ser renumerado de acordo com a modelagem adotada.}
}
\value{
retorna um vetor contendo as restrições de equilíbrio.
}
\description{
Gera as restrições de equilíbrio do modelo, ou seja, todo veículo que chega em um ponto de passagem da rota deve partir para outro ponto.
}
\examples{
# Exemplo de uso da função
restricoes_equilibrio <- gerar_restricoes_equilibrio(5,5,3,4)
print(restricoes_equilibrio)

restricoes_equilibrio1 <- gerar_restricoes_equilibrio(5,5,3)
print(restricoes_equilibrio1)

restricoes_equilibrio2 <- gerar_restricoes_equilibrio(1,5,3)
print(restricoes_equilibrio2)
restricoes_equilibrio3 <- gerar_restricoes_equilibrio(5,1,3)
print(restricoes_equilibrio3)

}
\author{
Luciane Ferreira Alcoforado
}
