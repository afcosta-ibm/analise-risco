#' Lista no. 6 – Risco de custo e prazo
#' Um novo prédio deve ser construído por um consorcio para um cliente. 
#' O projeto pode ser dividido em 7 seções como mostrado abaixo. 
#' O cliente deseja ver o resultado da análise de risco do prazo e do custo e 
#' da interdependência entre prazo e custo da obra.
#' 
#' Seção 1 – Projeto
#' O projeto detalhado demora (14,16,21) semanas, porém o arquiteto imagina que 
#' existe uma chance de 20% do cliente exigir alterações no projeto que 
#' implicam num prazo adicional de (3,4,6) semanas. O grupo de arquitetura 
#' cobra 160.000 fixo, mas requer um adicional de 12.000 por semana para 
#' qualquer tipo de retrabalho.

library(triangle)
# library(igraph)

predio.secao.1 <- predio_secao_1 <- predioSecao1 <- function(){
  
  print(' calculando a secao 1 do predio ')
  
  # number of samples
  NumberOfSamples <- 3000
  
  # prazos para a secao 1 do predio
  prazosSecao1 <- vector(length = NumberOfSamples)
  custosSecao1 <- vector(length = NumberOfSamples)
  
  # sorteia os cenarios de trabalho e retrabalho
  CenariosSemRetrabalho <- rtriangle(NumberOfSamples, 14, 21, 16)
  CenariosRetrabalho <- rtriangle(NumberOfSamples, 3, 6, 4)
  
  # sort dos eventos de retrabalho
  # 0 - com retrabalho
  # 1 - sem retrabalho
  sorteioEventoRetrabalho <- rbinom(NumberOfSamples, 1, 0.8)
  
  
  for(i in 1:NumberOfSamples){
    
    if(sorteioEventoRetrabalho[i] == 1){
      # sem retrabalho
      prazosSecao1[i] <- CenariosSemRetrabalho[i]
      # custo fixo de 160.000
      custosSecao1[i] <- 160000
      
    } else {
      # com retrabalho, soma os prazos do trabalho + retrabalho
      prazosSecao1[i] <- CenariosSemRetrabalho[i] + CenariosRetrabalho[i]
      # custo de 160.000 + 12.000 por semana do CenariosRetrabalho[i]
      # nao foi levado em consideracao o quebrado das semanas
      # usamos o floor(x)
      custosSecao1[i] <- 160000 + (12000 * floor(CenariosRetrabalho[i]))
      # custosSecao1[i] <- 160000 + (12000 * CenariosRetrabalho[i])
    }
  }
  
  # lista com os prazos e custos da secao 1
  lista.secao.1 <- list(prazos = prazosSecao1, custos = custosSecao1)
  
  return (lista.secao.1)

}