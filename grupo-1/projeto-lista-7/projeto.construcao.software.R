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
#' TSPOS - Teste de sistema pos retrabalho :: rtriangle(NumberOfSamples, 1, 3, 1)

library(triangle)
library(igraph)

# importa as funcoes necessarias

projeto.construcao.software <- projeto_construcao_software <- 
  projetoConstrucaoSoftware <- function(NumberOfSamples = 3000){
    
  print(NumberOfSamples)
  
  print(' montando os dados de prazo de todas as atividades ')

  # monta as linhas da matriz com os nomes corretos
  colunasMatrizAtividades <- c("PS", "PM1", "PM2", "PM3", "PM4", "CM1", "CM2"
                               ,"CM3", "CM4", "TM1", "TM2", "TM3", "TM4", "I"
                               ,"TS", "TA", "R1", "R2", "TSPOS")
  
  # inicializa a matriz de custos
  matrizPrazos <- matrix(nrow=NumberOfSamples, ncol=19,
                         dimnames = list(1:NumberOfSamples, 
                                         colunasMatrizAtividades))

  for(i in 1:NumberOfSamples){
    # PS
    matrizPrazos[,1] <- rtriangle(NumberOfSamples, 4, 7, 5)
    # PM1, PM2, PM3, PM4
    matrizPrazos[,2] <- rtriangle(NumberOfSamples, 2, 5, 2.5)
    matrizPrazos[,3] <- rtriangle(NumberOfSamples, 2, 5, 2.5)
    matrizPrazos[,4] <- rtriangle(NumberOfSamples, 2, 5, 2.5)
    matrizPrazos[,5] <- rtriangle(NumberOfSamples, 2, 5, 2.5)
    # CM1, CM2, CM3, CM4
    matrizPrazos[,6] <- rtriangle(NumberOfSamples, 4, 6.5, 5)
    matrizPrazos[,7] <- rtriangle(NumberOfSamples, 4, 6.5, 5)
    matrizPrazos[,8] <- rtriangle(NumberOfSamples, 4, 6.5, 5)
    matrizPrazos[,9] <- rtriangle(NumberOfSamples, 4, 6.5, 5)
    # TM1, TM2, TM3, TM4
    matrizPrazos[,10] <- rtriangle(NumberOfSamples, 1.5, 3, 2)
    matrizPrazos[,11] <- rtriangle(NumberOfSamples, 1.5, 3, 2)
    matrizPrazos[,12] <- rtriangle(NumberOfSamples, 1.5, 3, 2)
    matrizPrazos[,13] <- rtriangle(NumberOfSamples, 1.5, 3, 2)
    # I
    matrizPrazos[,14] <- rtriangle(NumberOfSamples, 4, 7, 5)
    # TS
    matrizPrazos[,15] <- rtriangle(NumberOfSamples, 2, 5, 2)
    # TA
    matrizPrazos[,16] <- rtriangle(NumberOfSamples, 1, 7, 1)
    # R1
    matrizPrazos[,17] <- rtriangle(NumberOfSamples, 2, 5, 3)
    # R2
    matrizPrazos[,18] <- rtriangle(NumberOfSamples, 1, 3, 1)
    # TSPOS
    matrizPrazos[,19] <- rtriangle(NumberOfSamples, 1, 3, 1)
  }

  return (matrizPrazos)
  
}