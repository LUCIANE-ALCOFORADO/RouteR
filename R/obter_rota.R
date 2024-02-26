


#' @Title Obter a rota para cada veículo
#'
#' @param vetor nomes das variáveis de valor 1 na solução do modelo (solução obtida utilizando por exemplo o pacote`lpSolve`)
#'
#' @return Dataframe contendo os índices ij de partida e chegada e o código do veículo correspondente à rota.
#' @examples
#'
#' # Exemplo de uso da função
#' vetor <- c("x121", "x122", "x125", "x211", "x212", "x213", "x311", "x312", "x313", "x135")
#' rotas_por_tipo <- obter_rotas(vetor)
#'
#' @export
obter_rota <- function(vetor) {
  # Obter os tipos de veículos identificados
  tipos_veiculos <- sort(unique(as.numeric(substr(vetor, 4, 4))))

  # Inicializar uma lista para armazenar as rotas de cada tipo de veículo
  rotas_por_tipo <- vector("list", length(tipos_veiculos))

  # Para cada tipo de veículo, filtrar o vetor e extrair as rotas
  for (tipo_veiculo in tipos_veiculos) {
    # Filtrar o vetor para obter apenas os elementos com o tipo de veículo especificado
    rotas <- vetor[grep(paste0("\\d", tipo_veiculo, "$"), vetor)]

    # Extrair os pontos de partida e chegada de cada rota
    rotas <- lapply(rotas, function(rota) {
      ponto_partida <- as.numeric(substr(rota, 2, 3))
      ponto_chegada <- as.numeric(substr(rota, 4, 5))
      return(list(Rota = ponto_partida, Veiculo = ponto_chegada))
    })

    # Verificar se o tipo de veículo está presente na lista de tipos de veículos
    if (tipo_veiculo %in% tipos_veiculos) {
      # Converter a lista de rotas em um dataframe e armazenar na lista de rotas por tipo de veículo
      rotas_por_tipo[[which(tipos_veiculos == tipo_veiculo)]] <- do.call(rbind.data.frame, rotas)
    }
  }

  # Criar uma tabela para cada tipo de veículo
  rotas_por_tipo <- lapply(rotas_por_tipo, function(rotas_tipo) {
    return(as.data.frame(rotas_tipo))
  })

  names(rotas_por_tipo) <- paste("TipoVeiculo", tipos_veiculos, sep = "_")
  tabela<- dplyr::bind_rows(rotas_por_tipo[1:length(tipos_veiculos)])
  return(tabela)
}





