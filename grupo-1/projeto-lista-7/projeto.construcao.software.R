#' Lista no. 7 – Risco de prazo com correlacao
#' 
#' Atividades:
#' PS - Projeto de sistema :: rtriangle(NumberOfSamples, 4, 7, 5)
#' PM (x4) - Projeto de módulo  :: rtriangle(NumberOfSamples, 2, 5, 2.5)
#' CM (x4) - Codificação do módulo :: rtriangle(NumberOfSamples, 4, 6.5, 5)
#' TM (x4) - Teste do módulo :: rtriangle(NumberOfSamples, 1.5, 3, 2)
#' I - Integração :: rtriangle(NumberOfSamples, 4, 7, 5)
#' TS - Teste do sistema :: rtriangle(NumberOfSamples, 2, 5, 2)
#' TA - Teste de aceitação :: rtriangle(NumberOfSamples, 1, 7, 1)
#' 
#' R1 - Primeiro Retrabalho :: rtriangle(NumberOfSamples, 2, 5, 3)
#' R2 - Segundo Retrabalho :: rtriangle(NumberOfSamples, 1, 3, 1)
#' TSR1 - Teste de sistema pos retrabalho 1 :: rtriangle(NumberOfSamples, 1, 3, 1)
#' TSR2 - Teste de sistema pos retrabalho 2 :: rtriangle(NumberOfSamples, 1, 3, 1)

library(triangle)
library(igraph)

# importa as funcoes necessarias

projeto.construcao.software <- projeto_construcao_software <- 
  projetoConstrucaoSoftware <- function(NumberOfSamples = 3000){
    
  print(NumberOfSamples)
  
  print(' montando os dados de prazo de todas as atividades ')

  # monta as colunas da matriz com os nomes corretos
  colunasMatrizAtividades <- c("PS", "PM1", "PM2", "PM3", "PM4", "CM1", "CM2"
                               ,"CM3", "CM4", "TM1", "TM2", "TM3", "TM4", "I"
                               ,"TS", "TA", "R1", "TSR1", "R2", "TSR2", "FIM",
                               "EVR1", "EVR2")
  
  # inicializa a matriz de custos
  matrizCenarios <- matrix(nrow=NumberOfSamples, ncol=23,
                         dimnames = list(1:NumberOfSamples, 
                                         colunasMatrizAtividades))

  # PS
  matrizCenarios[,1] <- rtriangle(NumberOfSamples, 4, 7, 5)
  # PM1, PM2, PM3, PM4
  matrizCenarios[,2] <- rtriangle(NumberOfSamples, 2, 5, 2.5)
  matrizCenarios[,3] <- rtriangle(NumberOfSamples, 2, 5, 2.5)
  matrizCenarios[,4] <- rtriangle(NumberOfSamples, 2, 5, 2.5)
  matrizCenarios[,5] <- rtriangle(NumberOfSamples, 2, 5, 2.5)
  # CM1, CM2, CM3, CM4
  matrizCenarios[,6] <- rtriangle(NumberOfSamples, 4, 6.5, 5)
  matrizCenarios[,7] <- rtriangle(NumberOfSamples, 4, 6.5, 5)
  matrizCenarios[,8] <- rtriangle(NumberOfSamples, 4, 6.5, 5)
  matrizCenarios[,9] <- rtriangle(NumberOfSamples, 4, 6.5, 5)
  # TM1, TM2, TM3, TM4
  matrizCenarios[,10] <- rtriangle(NumberOfSamples, 1.5, 3, 2)
  matrizCenarios[,11] <- rtriangle(NumberOfSamples, 1.5, 3, 2)
  matrizCenarios[,12] <- rtriangle(NumberOfSamples, 1.5, 3, 2)
  matrizCenarios[,13] <- rtriangle(NumberOfSamples, 1.5, 3, 2)
  # I
  matrizCenarios[,14] <- rtriangle(NumberOfSamples, 4, 7, 5)
  # TS
  matrizCenarios[,15] <- rtriangle(NumberOfSamples, 2, 5, 2)
  # TA
  matrizCenarios[,16] <- rtriangle(NumberOfSamples, 1, 7, 1)
  # R1
  matrizCenarios[,17] <- rtriangle(NumberOfSamples, 2, 5, 3)
  # TSR1
  matrizCenarios[,18] <- rtriangle(NumberOfSamples, 1, 3, 1)
  # R2
  matrizCenarios[,19] <- rtriangle(NumberOfSamples, 1, 3, 1)
  # TSR2
  matrizCenarios[,20] <- rtriangle(NumberOfSamples, 1, 3, 1)
  # FIM
  matrizCenarios[,21] <- 0
  
  # sorteio o retrabalho 1 onde o sorteio "1 = com retrabalho"
  matrizCenarios[,22] <- rbinom(n = NumberOfSamples, size = 1, prob = 0.3)
  # sorteio o retrabalho 2 onde o sorteio "1 = com retrabalho"
  matrizCenarios[,23] <- rbinom(n = NumberOfSamples, size = 1, prob = 0.05)
  
  return (matrizCenarios)
  
}