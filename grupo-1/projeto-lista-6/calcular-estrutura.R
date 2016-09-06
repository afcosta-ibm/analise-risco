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

calcular.estrutura <- calcular_estrutura <- calcularEstrutura <- function(numberOfSamples){
  
  print('Calculo da estrutura do predio')

  # Os componentes estruturais do prédio (pisos, pilares, cobertura) podem ser
  # iniciados, dependendo do tempo (3, 4, 6) semanas após o término do trabalho 
  # da fundação.
  espera.prazo <- rtriangle(n=numberOfSamples, a=3, b=6, c=4)

  # Cada piso custa (4.700, 5.200, 5.500) por semana de trabalho e 
  # (17.200, 17.500, 18.000) de material, dependente do projeto final
  # detalhado. 
  
  piso1.prazo <- rtriangle(n=numberOfSamples, a=4, b=6, c=4.5)
  piso1.custo <- rtriangle(n=numberOfSamples, a=4700, b=5200, c=5000)
  piso1.material <- rtriangle(n=numberOfSamples, a=17200, b=18000, c=17500)
  
  piso2.prazo <- rtriangle(n=numberOfSamples, a=4, b=6, c=4.5)
  piso2.custo <- rtriangle(n=numberOfSamples, a=4700, b=5200, c=5000)
  piso2.material <- rtriangle(n=numberOfSamples, a=17200, b=18000, c=17500)
  
  piso3.prazo <- rtriangle(n=numberOfSamples, a=4, b=6, c=4.5)
  piso3.custo <- rtriangle(n=numberOfSamples, a=4700, b=5200, c=5000)
  piso3.material <- rtriangle(n=numberOfSamples, a=17200, b=18000, c=17500)
  
  # O telhado tomará (7,8,10) semanas por um preço fixo de 172.000.
  telhado.prazo <- rtriangle(n=numberOfSamples, a=7, b=10, c=8)
  telhado.custo <- rep(times=numberOfSamples, x=172000)

  result <- list(espera.prazo=espera.prazo, 
                 piso1.prazo=piso1.prazo,
                 piso1.custo=piso1.custo,
                 piso1.material=piso1.material,
                 piso2.prazo=piso1.prazo,
                 piso2.custo=piso1.custo,
                 piso2.material=piso1.material,
                 piso3.prazo=piso1.prazo,
                 piso3.custo=piso1.custo,
                 piso3.material=piso1.material,
                 telhado.prazo=telhado.prazo,
                 telhado.custo=telhado.custo)
  
  return (result)
  
}
