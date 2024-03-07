require(RouteR)
require(magrittr)
Dados<-readr::read_delim(
  "https://raw.githubusercontent.com/LUCIANE-ALCOFORADO/RouteR/master/Planilhas/formulacao_1_veiculo.csv?token=GHSAT0AAAAAACOXBG7FTIJTXCFOLEK34WLMZPGI5IA",                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
Rota=gerar_rota(Dados)
Rota_ordenada=ordenar_rota(Rota$rota$var)
Rota_ordenada

Pane=sample(2:(length(Rota$rota$var)-1),1)
Rota_ordenada[Pane,]
Rota_ordenada[Pane:length(Rota$rota$var),]
i<-sort(c(1,Rota_ordenada[Pane:length(Rota$rota$var),]$origem))
i
indices<-tidyr::expand_grid(i=i , j=i, 
        k = 1) %>% dplyr::filter(i!=j)
var_nova=tidyr::expand_grid(i=i , j=i, 
        k = 1) %>% dplyr::filter(i!=j)%>% dplyr::mutate(x = paste0("x", 
        i, j, k))
Dados_novos=Dados %>% dplyr::select(all_of(c("cod",var_nova$x,"direcao","b"))) 
Dados_novos$todas_nulas <- apply(Dados_novos[, var_nova$x], 1, function(row) all(row == 0))
Dados_novos<- Dados_novos%>%dplyr::filter(!todas_nulas) #até aqui temos as variáveis que importam, eliminamos as linhas nulas
Dados_novos<- Dados_novos %>% dplyr::select(-todas_nulas) %>% dplyr::filter(b<=2)
                                                                                                                  
Rota1<- gerar_rota(Dados_novos)
Rota_ordenada1=ordenar_rota(Rota1$rota$var)
Rota_ordenada1

gerar_pane<-function(rota_ordenada,semente=1){
  #dataframe com a rota ordenada contendo colunas $Veiculo, $Rota,$origem, $destino
  #semente para reproduzir resultado
  #Retorna lista$pontos com os pontos não visitados devido a pane
  #Retorna lista$var_nova com os indices da nova variável a ser considerada
  if(!is.data.frame(rota_ordenada)){
    return("dataframe com a rota ordenada contendo colunas $Veiculo, $Rota, $origem, $destino")
      }
#rota_ordenada<-ordenar_rota(rota_ordenada)
set.seed(semente)
Pane=sample(2:(length(rota_ordenada$origem)-1),1)
rota_realizada=rota_ordenada[1:(Pane-1),]
var_realizadas=paste0("x",rota_realizada$Rota,rota_realizada$Veiculo)
custo_realizado=sum(Dados[1,var_realizadas])
rota_ordenada[Pane,]
rota_ordenada[Pane:length(rota_ordenada$origem),]
i<-sort(c(1,rota_ordenada[Pane:length(rota_ordenada$origem),]$origem))
i
indices<-tidyr::expand_grid(i=i , j=i, 
        k = 1) %>% dplyr::filter(i!=j)
var_nova=tidyr::expand_grid(i=i , j=i, 
        k = 1) %>% dplyr::filter(i!=j)%>% dplyr::mutate(x = paste0("x", 
        i, j, k))
lista=list(parada=Pane, rota_realizada=rota_realizada, custo_realizado=custo_realizado,pontos=i[i!=1], var_nova=var_nova)
return(lista)
}

nova_rota<-function(var,Dados,n_subrota=2){
  #Recalcula a nova rota partindo da origem 1 com base em var
  Dados_novos=Dados %>% dplyr::select(all_of(c("cod",var$x,"direcao","b"))) 
  Dados_novos$todas_nulas <- apply(Dados_novos[, var$x], 1, function(row) all(row == 0))
  Dados_novos<- Dados_novos%>%dplyr::filter(!todas_nulas) #até aqui temos as variáveis que importam, eliminamos as linhas nulas
  Dados_novos<- Dados_novos %>% dplyr::select(-todas_nulas) %>% dplyr::filter(b<=n_subrota)
                                                                                                                  
Rota1<- gerar_rota(Dados_novos)
objetivo=Rota1$objetivo
Rota_ordenada1=ordenar_rota(Rota1$rota$var)
Rota_ordenada1


return(list(Rota=Rota_ordenada1,objetivo=objetivo))
}


#Exemplo
##Considerando a solução inicial para os Dados
Rota=gerar_rota(Dados)
custo_inicial=Rota$objetivo
Rota$objetivo
Rota_ordenada=ordenar_rota(Rota$rota$var)
Rota_ordenada
gerar_pane(Rota_ordenada, semente=45)
var=gerar_pane(Rota_ordenada,semente=45)$var
nova_rota(var,Dados)
Rota1=nova_rota(var,Dados)
custo_total=gerar_pane(Rota_ordenada, semente=45)$custo_realizado+nova_rota(var,Dados)$objetivo
custo_total
tb_custo=data.frame(rota0=paste0(Rota_ordenada$Rota,collapse = "-"),custo_inicial,
                    rota1=paste0(Rota1$Rota$Rota,collapse = "-"), custo_total)
knitr::kable(tb_custo)

#Simulação de pane num certo trecho da rota
#
for(i=1:n)
