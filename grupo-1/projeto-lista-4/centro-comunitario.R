#' A equipe do Prefeito está elaborando uma proposta para um novo centro 
#' comunitário de uma cidade. A equipe da gerência do projeto dividiu o escopo 
#' do projeto em 7 pacotes de trabalho e uma equipe de especialistas em 
#' construção definiu estimativas de 3 pontos para cada um dos pacotes de 
#' trabalho.
#' Os valores resultantes deste trabalho estão mostrados abaixo
#' (todos os valores em USD$ 1000).

#' Pacote 1 - Planejamento inicial:
#' O valor estimado é de (15;17;19).
#' O fato do Prefeito não dispor de maioria na Câmara implica numa chance de 50% 
#' do plano inicial ser rejeitado, o que ocasionaria alterações no projeto que 
#' elevariam este custo para (20;22;25).

#' Pacote 2 - Terraplanagem: O cenário mais favorável, com 75% de chance, é 
#' aquele em que não serão encontrados problemas de infiltração do lençol 
#' freático e o custo de terraplanagem estimado é de (41;42;47). 
#' No caso de infiltração este custo subirá para (45;47;50).

#' Pacote 3 - Material: custo estimado (100,105,110).
#' Pacote 4 - Mão de obra: valor estimado (40,45,52).
#' Pacote 5 - Aluguel de equipamentos: custo estimado (35,36,40).
#' Pacote 6 - Acabamento e jardinagem: custo estimado (25,26,27).
#' Pacote 7 - Administração da obra: custo estimado (15,17,19).

library(triangle)

centro.Comunitario <- centro_comunitario <- centroComunitario <- function(){
  # number of samples
  NumberOfSamples <- 3000
  # array de retorno com os custos
  retorno <- vector(length = NumberOfSamples)
  # custos cumulativos
  custoTotalCentroComunitario <- 0
  custoPacote1 <- 0
  custoPacote2 <- 0
  custoPacote3 <- 0
  custoPacote4 <- 0
  custoPacote5 <- 0
  custoPacote6 <- 0
  custoPacote7 <- 0
  
  for(i in 1:NumberOfSamples){
    
    # calcula o custo do pacote 1
    # sorteia a moeda sem vicio: 50% e 50% para 0 e 1
    planoAprovadoCamara <- sample(x=c(0,1), size=1, replace=TRUE, prob=c(0.5,0.5))
    
    if(planoAprovadoCamara == 0){
      # plano rejeitado
      custoPacote1 <- rtriangle (1, 20, 25, 22)
    } else if (planoAprovadoCamara == 1) {
      # plano aceito
      custoPacote1 <- rtriangle (1, 15, 19, 17)
    }
    
    # calcula o custo do pacote 2
    # sorteia a moeda com vicio de 75% para NÃO tem infiltração - 0
    # e 25% tem infiltração - 1
    possuiInfiltracao <- sample(x=c(0,1), size=1, replace=TRUE, prob=c(0.75,0.25))

    if(possuiInfiltracao == 0){
      # não tem infiltração 75% de chance
      custoPacote2 <- rtriangle(1,41,47,42)
      
    } else if (possuiInfiltracao == 1){
      # tem infiltração 25% de chance
      custoPacote2 <- rtriangle(1,45,50,47)
    }
    
    # calcula o custo do pacote 3
    #' Pacote 3 - Material: custo estimado (100,105,110).
    custoPacote3 <- rtriangle (1, 100, 110, 105)
    
    # calcula o custo do pacote 4
    #' Pacote 4 - Mão de obra: valor estimado (40,45,52).
    custoPacote4 <-  rtriangle (1, 40, 52, 45)
    
    # calcula o custo do pacote 5
    #' Pacote 5 - Aluguel de equipamentos: custo estimado (35,36,40).
    custoPacote5 <- rtriangle (1, 35, 40, 36)
    
    # calcula o custo do pacote 6
    #' Pacote 6 - Acabamento e jardinagem: custo estimado (25,26,27).
    custoPacote6 <- rtriangle (1, 25, 27, 26)
    
    # calcula o custo do pacote 7
    #' Pacote 7 - Administração da obra: custo estimado (15,17,19).
    custoPacote7 <- rtriangle (1, 15, 19, 17)
    
    # atualizo o custo do centro comunitario 
    custoTotalCentroComunitario <- custoPacote1 + custoPacote2 + custoPacote3 
      + custoPacote4 + custoPacote5 + custoPacote6 + custoPacote7 
    
    retorno[i] <- custoTotalCentroComunitario
  }
  
  return (retorno)
}