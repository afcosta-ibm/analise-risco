#' Lista no. 6 – Risco de custo e prazo
#' Um novo prédio deve ser construído por um consorcio para um cliente. 
#' O projeto pode ser dividido em 7 seções como mostrado abaixo. 
#' O cliente deseja ver o resultado da análise de risco do prazo e do custo e 
#' da interdependência entre prazo e custo da obra.
#' 
#' Seção 3 – Fundações
#' O trabalho de fundação pode ser iniciado assim que a terraplanagem termine 
#' e levará (6,7,8) semanas. O custo estimado é de (2.800,3.300,3.300) por 
#' semana para mão de obra e de (37.000,38.500,40.000) de materiais.

library(triangle)

predio.secao.3 <- predio_secao_3 <- predioSecao3 <- 
  function(NumberOfSamples = 3000){
  
  print(' calculando a secao 3 do predio :: Fundacoes ')
  
  # prazos para a secao 3 do predio
  prazosSecao3 <- vector(length = NumberOfSamples)
  custosSecao3 <- vector(length = NumberOfSamples)
  
  # sorteia os cenarios de trabalho e retrabalho
  CenariosPrazos <- rtriangle(NumberOfSamples, 6, 8, 7)
  CenariosCustos <- rtriangle(NumberOfSamples, 2800, 3300, 3300)
  CenariosMateriais <- rtriangle(NumberOfSamples, 37000, 40000, 38500)

  # o prazo da fundacao nao varia
  prazosSecao3 <- CenariosPrazos
    
  for(i in 1:NumberOfSamples){
    # custo = (prazo * custo) + materiais
    custosSecao3[i] <- ((CenariosPrazos[i] * CenariosCustos[i]) 
                                                      + CenariosMateriais[i])
  }
  
  # lista com os prazos e custos da secao 3
  lista.secao.3 <- list(prazos = prazosSecao3, custos = custosSecao3)
  
  return (lista.secao.3)

}