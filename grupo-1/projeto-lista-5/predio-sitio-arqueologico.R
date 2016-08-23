#' Um prédio deve ser construido perto de um sítio arqueológico. A tabela 
#' abaixo mostra as atividades a serem desenvolvidas de forma sequencial, uma de
#' cada vez. 
#' 
#' Tarefa		       Min		Mp		 Max
#' Arqueologia     3.5    4.3    5.0         
#' Escavação       9.0    10.9  12.0  
#' Formas          1.5    2.2    3.2
#' Fundação        5.0    6.7    7.5
#' Paredes        15.0   16.7   18.0
#' Telhado         6.5    7.6    9.0
#' 
#' A tabela abaixo mostra o impacto (%) na duração de cada atividade devido a
#' cada condição de tempo.
#' 
#' Impacto (%) na duração devido as condições do tempo
#'   MR     Ruim     Normal     Bom     MB
#'   40     28         0         0      -2
#'   30     20         0        -6     -10
#'   10     04         0         0      -3
#'   40     25         0       -12     -18
#'   10     04         0         0      -2
#'   20     08         0        -4      -6
#'   
#' Os especialistas prevêm que as probabilidades da ocorrência de cada tipo 
#' de tempo durante a obra são: 
#' 
#'  Tempo    Probabilidade
#'    MR       0.125  
#'   Ruim      0.3125  
#' Normal      0.25  
#'   Bom       0.1875  
#'    MB       0.125
#' 

library(triangle)

predio.Sitio.Arqueologico <- predio_sitio_arqueologico <- 
  predioSitioArqueologico <- function(){
  
  # number of samples
  NumberOfSamples <- 3000
  
  # array de retorno com as duracoes
  #retorno <- vector(length = NumberOfSamples)
  
  #arrays de consulta futura para ver maior impacto
  prazosArqueologia <- vector(length = NumberOfSamples)
  prazosEscavacao <- vector(length = NumberOfSamples)
  prazosFormas <- vector(length = NumberOfSamples)
  prazosFundacao <- vector(length = NumberOfSamples)
  prazosParedes <- vector(length = NumberOfSamples)
  prazosTelhado <- vector(length = NumberOfSamples)
  prazoTotal <- vector(length = NumberOfSamples)
  
  # duracoes cumulativos
  tempoTotalConstrucao <- 0
  tempoArqueologia <- 0
  tempoEscavacao <- 0
  tempoFormas <- 0
  tempoFundacao <- 0
  tempoParedes <- 0
  tempoTelhado <- 0

  for(i in 1:NumberOfSamples){

    # calcula o tempo da atividade Arqueologia
    # sorteia o multiplicador de impacto na duracao da atividade dado o tempo
    # % de impacto
    # 1.4, 1.28, 1, 1, 0.98
    # probabilidades
    # 0.125, 0.3125, 0.25, 0.1875, 0.125
    multiplicadorArqueologia <- sample(x=c(1.4, 1.28, 1, 1, 0.98), size=1, 
                                    replace=TRUE, 
                                    prob=c(0.125, 0.3125, 0.25, 0.1875, 0.125))
    # multiplica já o tempo da atividade com seu % de impacto sorteado
    tempoArqueologia <- rtriangle (1, 3.5, 5.0, 4.3) * multiplicadorArqueologia
    # armazena para consulta futura os prazos das atividades
    prazosArqueologia[i] <- tempoArqueologia

    # --------------------------------------------------------------------#

    # calcula o tempo da atividade Escavação
    # sorteia o multiplicador de impacto na duracao da atividade dado o tempo
    # % de impacto
    # 1.3, 1.2, 1, 0.94, 0.90
    # probabilidades
    # 0.125, 0.3125, 0.25, 0.1875, 0.125
    multiplicadorEscavacao <- sample(x=c(1.3, 1.2, 1, 0.94, 0.90), size=1, 
                                    replace=TRUE, 
                                    prob=c(0.125, 0.3125, 0.25, 0.1875, 0.125))
    # multiplica já o tempo da atividade com seu % de impacto sorteado
    tempoEscavacao <- (rtriangle (1, 9.0, 12.0, 10.9) * multiplicadorEscavacao)
    # armazena para consulta futura os prazos das atividades
    prazosEscavacao[i] <- tempoEscavacao

    # --------------------------------------------------------------------#
    
    # calcula o tempo da atividade Formas
    # sorteia o multiplicador de impacto na duracao da atividade dado o tempo
    # % de impacto
    # 1.1, 1.04, 1, 1, 0.97
    # probabilidades
    # 0.125, 0.3125, 0.25, 0.1875, 0.125
    multiplicadorFormas <- sample(x=c(1.1, 1.04, 1, 1, 0.97), size=1, 
                                     replace=TRUE, 
                                     prob=c(0.125, 0.3125, 0.25, 0.1875, 0.125))
    # multiplica já o tempo da atividade com seu % de impacto sorteado
    tempoFormas <- (rtriangle (1, 1.5, 3.2, 2.2) * multiplicadorFormas)
    # armazena para consulta futura os prazos das atividades
    prazosFormas[i] <- tempoFormas

    # --------------------------------------------------------------------#
    
    # calcula o tempo da atividade Fundação
    # sorteia o multiplicador de impacto na duracao da atividade dado o tempo
    # % de impacto
    # 1.4, 1.25, 1, 0.88, 0.82
    # probabilidades
    # 0.125, 0.3125, 0.25, 0.1875, 0.125
    multiplicadorFundacao <- sample(x=c(1.4, 1.25, 1, 0.88, 0.82), size=1, 
                                  replace=TRUE, 
                                  prob=c(0.125, 0.3125, 0.25, 0.1875, 0.125))
    # multiplica já o tempo da atividade com seu % de impacto sorteado
    tempoFundacao <- (rtriangle (1, 5.0, 7.5, 6.7) * multiplicadorFundacao)
    # armazena para consulta futura os prazos das atividades
    prazosFundacao[i] <- tempoFundacao

    # --------------------------------------------------------------------#
    
    # calcula o tempo da atividade Paredes
    # sorteia o multiplicador de impacto na duracao da atividade dado o tempo
    # % de impacto
    # 1.1, 1.04, 1, 1, 0.98
    # probabilidades
    # 0.125, 0.3125, 0.25, 0.1875, 0.125
    multiplicadorParedes <- sample(x=c(1.1, 1.04, 1, 1, 0.98), size=1, 
                                    replace=TRUE, 
                                    prob=c(0.125, 0.3125, 0.25, 0.1875, 0.125))
    # multiplica já o tempo da atividade com seu % de impacto sorteado
    tempoParedes <- (rtriangle (1, 15.0, 18.0, 16.7) * multiplicadorParedes)
    # armazena para consulta futura os prazos das atividades
    prazosParedes[i] <- tempoParedes

    # --------------------------------------------------------------------#
    
    # calcula o tempo da atividade Telhado
    # sorteia o multiplicador de impacto na duracao da atividade dado o tempo
    # % de impacto 
    # 1.2, 1.08, 1, 0.96, 0.94
    # probabilidades
    # 0.125, 0.3125, 0.25, 0.1875, 0.125
    multiplicadorTelhado <- sample(x=c(1.2, 1.08, 1, 0.96, 0.94), size=1, 
                                   replace=TRUE, 
                                   prob=c(0.125, 0.3125, 0.25, 0.1875, 0.125))
    # multiplica já o tempo da atividade com seu % de impacto sorteado
    tempoTelhado <- (rtriangle (1, 15.0, 18.0, 16.7) * multiplicadorTelhado)
    # armazena para consulta futura os prazos das atividades
    prazosTelhado[i] <- tempoTelhado

    # --------------------------------------------------------------------#
    
    # atualizo o tempo total de construcao do predio
    tempoTotalConstrucao <- tempoArqueologia + tempoEscavacao + tempoFormas + 
        tempoFundacao + tempoParedes + tempoTelhado
    
    prazoTotal[i] <- tempoTotalConstrucao
  }

  # lista com as duracoes de todas as atividades + o tempo total de construcao
  lista.duracoes <- list(arqueologia = prazosArqueologia, 
                         escavacao = prazosEscavacao, 
                         formas = prazosFormas,
                         fundacao = prazosFundacao, 
                         paredes = prazosParedes,
                         telhado = prazosTelhado,
                         prazoTotal = prazoTotal)
  
  return (lista.duracoes)
}