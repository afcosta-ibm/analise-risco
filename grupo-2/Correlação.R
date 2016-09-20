#' Lista no. 7 Risco de prazo com correlacao
#' 
#' Atividades:
#' Projeto de sistema :: rtriangle(amostras, 4, 7, 5)
#' Projeto de módulo (x4)  :: rtriangle(amostras, 2, 5, 2.5)
#' Codificação do módulo (x4) :: rtriangle(amostras, 4, 6.5, 5)
#' Teste do módulo (x4) :: rtriangle(amostras, 1.5, 3, 2)
#' Integração :: rtriangle(amostras, 4, 7, 5)
#' Teste do sistema :: rtriangle(amostras, 2, 5, 2)
#' Teste de aceitação :: rtriangle(amostras, 1, 7, 1)
#' 
#' R1 - Primeiro Retrabalho :: rtriangle(amostras, 2, 5, 3)
#' R2 - Segundo Retrabalho :: rtriangle(amostras, 1, 3, 1)
#' TSR1 - Teste de sistema pos retrabalho 1 :: rtriangle(amostras, 1, 3, 1)
#' TSR2 - Teste de sistema pos retrabalho 2 :: rtriangle(amostras, 1, 3, 1)

library(triangle)

# importar funcoes

projeto.construcao.software <- projeto_construcao_software <- 
  projetoConstrucaoSoftware <- function(amostras = 3000){
    
  print(amostras)
  
  print(' montando os dados de prazo de todas as atividades ')

  # monta as colunas da matriz com os nomes corretos
  colunasMatrizAtividades <- c("PS", "PM1", "PM2", "PM3", "PM4", "CM1", "CM2"
                               ,"CM3", "CM4", "TM1", "TM2", "TM3", "TM4", "I"
                               ,"TS", "TA", "R1", "TSR1", "R2", "TSR2", "FIM",
                               "EVR1", "EVR2")
  
  # inicia a matriz de custos
  matrizCenarios <- matrix(nrow=amostras, ncol=23)
                        

 matrizCenarios = matrix(nrow=amostras,ncol=23)  
    matrizCenarios[,1] <- rtriangle(amostras, 4, 7, 5)
    matrizCenarios[,2] <- rtriangle(amostras, 2, 5, 2.5)
    matrizCenarios[,3] <- rtriangle(amostras, 2, 5, 2.5)
    matrizCenarios[,4] <- rtriangle(amostras, 2, 5, 2.5)
    matrizCenarios[,5] <- rtriangle(amostras, 2, 5, 2.5)
    matrizCenarios[,6] <- rtriangle(amostras, 4, 6.5, 5)
    matrizCenarios[,7] <- rtriangle(amostras, 4, 6.5, 5)
    matrizCenarios[,8] <- rtriangle(amostras, 4, 6.5, 5)
    matrizCenarios[,9] <- rtriangle(amostras, 4, 6.5, 5)
    matrizCenarios[,10] <- rtriangle(amostras, 1.5, 3, 2)
    matrizCenarios[,11] <- rtriangle(amostras, 1.5, 3, 2)
    matrizCenarios[,12] <- rtriangle(amostras, 1.5, 3, 2)
    matrizCenarios[,13] <- rtriangle(amostras, 1.5, 3, 2)
    matrizCenarios[,14] <- rtriangle(amostras, 4, 7, 5)
    matrizCenarios[,15] <- rtriangle(amostras, 2, 5, 2)
    matrizCenarios[,16] <- rtriangle(amostras, 1, 7, 1)
    matrizCenarios[,17] <- rtriangle(amostras, 2, 5, 3)
    matrizCenarios[,18] <- rtriangle(amostras, 1, 3, 1)
    matrizCenarios[,19] <- rtriangle(amostras, 1, 3, 1)
    matrizCenarios[,20] <- rtriangle(amostras, 1, 3, 1)
	
	#FIM
	
    matrizCenarios[,21] <-0
	
	#sortear retrabalho 1 onde o sorteio"1 = retrabalho"
	matrizCenarios[,22] <- rbinom(n = amostras, size = 1, prob = 0.3)
	
    # sorteio o retrabalho 2 onde o sorteio "1 = retrabalho"
    matrizCenarios[,23] <- rbinom(n = amostras, size = 1, prob = 0.05)
  
    return (matrizCenarios)
  }