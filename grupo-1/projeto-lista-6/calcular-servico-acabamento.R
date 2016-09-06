#' Seção 6 – Serviços e acabamento
#' 
#' Os serviços (encanamento, eletricidade, cabeamento para computadores) e o 
#' acabamento podem ser iniciados assim que cada piso estiver terminado. 
#' 
#' O arquiteto foi solicitado a fazer duas estimativas para o acabamento: 
#' a) um acabamento espartano e funcional e 
#' b) opulento e vistoso. 
#' 
#' O cliente deseja a opção (b) mas reconhece que existe uma chance de 5% 
#' dela ser vetada pelo Conselho Diretor da empresa.
#'  
#' A opção espartana custa (92.000,95.000,107.000) enquanto que a opulenta 
#' custaria (106.000,112.000,114.000). 
#' 
#' Os serviços devem tomar (8,10,13) semanas, enquanto que o acabamento 
#' demora (9,11,13) para cada piso.
#' 

library(triangle)

calcular.servico.acabamento <- calcular_servico_acabamento <- 
  calcularServicoAcabamento <- function(numberOfSamples){
    
    print('Calculo do servico de acabamento do predio')
    
    # O arquiteto foi solicitado a fazer duas estimativas para o acabamento: 
    # a) um acabamento espartano e funcional e 
    # b) opulento e vistoso. 
    # O cliente deseja a opção (b) mas reconhece que existe uma chance de 5% 
    # dela ser vetada pelo Conselho Diretor da empresa.    
    vetor.unitario <- rep(times=numberOfSamples, x=1)
    fator.espartano <- rbinom(n=numberOfSamples, 1, 0.05)
    fator.opulento <- vetor.unitario - fator.espartano

    #' A opção espartana custa (92.000,95.000,107.000) enquanto que a opulenta 
    #' custaria (106.000,112.000,114.000). 
    custo.espartando <- rtriangle(n=numberOfSamples, a=92000, b=107000, c=95000)
    custo.opulento <- rtriangle(n=numberOfSamples, a=106000, b=114000, c=112000)
    
    custo.espartando.ajustado <- custo.espartando * fator.espartano
    custo.opulento.ajustado <- custo.opulento * fator.opulento
    custo <- custo.espartando.ajustado + custo.opulento.ajustado
    
    # Os serviços devem tomar (8,10,13) semanas, enquanto que o acabamento 
    # demora (9,11,13) para cada piso.
    prazo.servico <- rtriangle(n=numberOfSamples, a=8, b=13, c=10)
    prazo.acabamento <- rtriangle(n=numberOfSamples, a=9, b=13, c=11)
    
    prazo <- prazo.servico + prazo.acabamento

    result <- list(custo=custo, prazo=prazo)
    
    return (result)
  }