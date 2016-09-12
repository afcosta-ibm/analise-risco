#' Seção 4 – Estrutura
#' 
#' Os componentes estruturais do prédio (pisos, pilares, cobertura) 
#' podem ser iniciados, dependendo do tempo (3, 4, 6) semanas após o 
#' término do trabalho da fundação. 
#' 
#' O prédio possui três pisos iguais, sendo que o construtor estima 
#' que cada um deles pode ser construído em (4, 4.5, 6) semanas dependendo 
#' das condições do tempo. 
#' 
#' Cada piso custa (4.700, 5.200, 5.500) por semana de trabalho e 
#' (17.200, 17.500, 18.000) de material, dependente do projeto final
#' detalhado. 
#' 
#' O telhado tomará (7,8,10) semanas por um preço fixo de 172.000.
#' 

library(triangle)

calcular.estrutura <- calcular_estrutura <- calcularEstrutura <- 
  function(numberOfSamples){
  
  print(' Calculo da estrutura do predio :: secao 4 ')

  # Os componentes estruturais do prédio (pisos, pilares, cobertura) podem ser
  # iniciados, dependendo do tempo (3, 4, 6) semanas após o término do trabalho 
  # da fundação.
  espera.prazo <- rtriangle(n=numberOfSamples, a=3, b=6, c=4)

  # Cada piso custa (4.700, 5.200, 5.500) por semana de trabalho e 
  # (17.200, 17.500, 18.000) de material, dependente do projeto final
  # detalhado. 
  
  piso1.prazo <- rtriangle(n=numberOfSamples, a=4, b=6, c=4.5)
  piso1.mao.de.obra <- rtriangle(n=numberOfSamples, a=4700, b=5200, c=5000)
  piso1.material <- rtriangle(n=numberOfSamples, a=17200, b=18000, c=17500)
  
  piso2.prazo <- rtriangle(n=numberOfSamples, a=4, b=6, c=4.5)
  piso2.mao.de.obra <- rtriangle(n=numberOfSamples, a=4700, b=5200, c=5000)
  piso2.material <- rtriangle(n=numberOfSamples, a=17200, b=18000, c=17500)
  
  piso3.prazo <- rtriangle(n=numberOfSamples, a=4, b=6, c=4.5)
  piso3.mao.de.obra <- rtriangle(n=numberOfSamples, a=4700, b=5200, c=5000)
  piso3.material <- rtriangle(n=numberOfSamples, a=17200, b=18000, c=17500)
  
  # O telhado tomará (7,8,10) semanas por um preço fixo de 172.000.
  telhado.prazo <- rtriangle(n=numberOfSamples, a=7, b=10, c=8)
  telhado.mao.de.obra <- rep(times=numberOfSamples, x=172000)

  prazo <- espera.prazo + 
    piso1.prazo + 
    piso2.prazo + 
    piso3.prazo + 
    telhado.prazo
    
  custo.material <- 
    piso1.material + 
    piso2.material + 
    piso3.material
  
  custo.mao.de.obra <- 
    piso1.mao.de.obra + 
    piso2.mao.de.obra + 
    piso3.mao.de.obra + 
    telhado.mao.de.obra
  
  custo <- custo.material + custo.mao.de.obra

  result <- list(custos=custo, prazos=prazo)
  
  return (result)  
}
