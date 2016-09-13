#' Seção 7- Finalização
#' 
#' São necessárias duas semanas depois de todo o trabalho ser completado para 
#' a limpeza do local e teste dos serviços a um custo de 4.000. 
#' 
#' Imagina-se que existe uma chance de 40% da empresa contratada para os 
#' serviços ser chamada para acertos que tomarão (0.2,2,5) semanas. 
#' 
#' Além disto, existe uma chance de 5% dela ser chamada uma segunda vez 
#' resultando em mais um atraso de (0.5,1,1.5) semanas.
#' 

library(triangle)

calcular.finalizacao <- calcular_finalizacao <- 
  calcularFinalizacao <- function(numberOfSamples){
  
  print(' Calculo da finalizacao do predio :: secao 7 ')

  # São necessárias duas semanas depois de todo o trabalho ser completado para 
  # a limpeza do local e teste dos serviços a um custo de 4.000. 
  prazo.normal <- rep(times=numberOfSamples, x=2)
  custo <- rep(times=numberOfSamples, x=4000)
    
  # Imagina-se que existe uma chance de 40% da empresa contratada para os 
  # serviços ser chamada para acertos que tomarão (0.2,2,5) semanas. 
  fator1 <- rbinom(n=numberOfSamples, 1, 0.4)
  prazo.adicional1 <- rtriangle(n=numberOfSamples, a=.2, b=5, c=2)
  prazo.adicional1.ajustado <- prazo.adicional1 * fator1
  
  # Além disto, existe uma chance de 5% dela ser chamada uma segunda vez 
  # resultando em mais um atraso de (0.5,1,1.5) semanas.
  fator2 <- rbinom(n=numberOfSamples, 1, 0.05)
  prazo.adicional2 <- rtriangle(n=numberOfSamples, a=.5, b=1.5, c=1)
  prazo.adicional2.ajustado <- prazo.adicional2 * fator2
  
  prazo <- prazo.normal + 
    prazo.adicional1.ajustado + 
    prazo.adicional2.ajustado
  
  result <- list(custos=custo, prazos=prazo, eventos1=fator1, eventos2=fator2)

  return (result)
}