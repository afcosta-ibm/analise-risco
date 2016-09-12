#' Lista no. 6 – Risco de custo e prazo
#' Um novo prédio deve ser construído por um consorcio para um cliente. 
#' O projeto pode ser dividido em 7 seções como mostrado abaixo. 
#' O cliente deseja ver o resultado da análise de risco do prazo e do custo e 
#' da interdependência entre prazo e custo da obra.
#' 
#' Seção 2 – Terraplanagem
#' O local deverá ser nivelado. Esta tarefa pode começar imediatamente após a 
#' assinatura do contrato. A terraplanagem vai demorar (3,4,7) semanas a um 
#' custo de (4.200,4.500,4.700) por semana. Existe um risco que a terraplanagem 
#' revele a presença de artefatos que irão requerer uma inspeção arqueológica 
#' complexa antes que o trabalho de construção possa continuar. Conhecimento 
#' sobre o local indica que existe uma probabilidade de 30% de encontrar 
#' artefatos arqueológicos, o que neste caso, implicaria em um tempo de 
#' inspeção de (8,10,14) semanas.

library(triangle)

predio.secao.2 <- predio_secao_2 <- predioSecao2 <- 
  function(NumberOfSamples = 3000){
  
  print(' calculando a secao 2 do predio :: Terraplanagem ')
  
  # prazos para a secao 2 do predio
  prazosSecao2 <- vector(length = NumberOfSamples)
  custosSecao2 <- vector(length = NumberOfSamples)
  
  # sorteia os cenarios de terraplanagem 
  # os cenarios de terraplanagem com inspecao arqueologica
  # e os cenarios de custo da terraplanagem
  CenariosSemInspecaoArqueologica <- rtriangle(NumberOfSamples, 3, 7, 4)
  CenariosComInspecaoArqueologica <- rtriangle(NumberOfSamples, 8, 14, 10)
  CenariosCustoTerraplanagem <- rtriangle(NumberOfSamples, 4200, 4700, 4500)
  
  # sort dos eventos de inspecao arqueologica
  # 0 - com inspecao arqueologica
  # 1 - sem inspecao arqueologica
  sorteioEventoInspecaoArqueologica <- rbinom(NumberOfSamples, 1, 0.7)
  
  
  for(i in 1:NumberOfSamples){
    
    if(sorteioEventoInspecaoArqueologica[i] == 1){
      # sem inspecao arqueologica
      prazosSecao2[i] <- CenariosSemInspecaoArqueologica[i]
      
      # prazo sem inspecao * custo da terraplanagem
      custosSecao2[i] <- (CenariosSemInspecaoArqueologica[i] 
                                            * CenariosCustoTerraplanagem[i])
      
    } else {
      # com inspecao arqueologica
      prazosSecao2[i] <- CenariosComInspecaoArqueologica[i]
      
      # prazo com inspecao * custo da terraplanagem
      custosSecao2[i] <- (CenariosComInspecaoArqueologica[i] 
                                            * CenariosCustoTerraplanagem[i])

    }
  }
  
  # lista com os prazos e custos da secao 2
  lista.secao.2 <- list(prazos = prazosSecao2, custos = custosSecao2, 
                        eventos = sorteioEventoInspecaoArqueologica)
  
  return (lista.secao.2)

}