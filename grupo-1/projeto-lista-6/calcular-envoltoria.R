#' Seção 5 – Envoltória
#' 
#' O trabalho na envoltória (paredes, janelas e portas externas) só pode de ser 
#' iniciado 3 semanas depois do primeiro piso ter sido terminado. 
#' 
#' A estimativa para o custo de material é de (36.000, 37.000, 40.000) por piso 
#' dependendo do projeto final de arquitetura. 
#' 
#' O piso térreo vai necessitar de portas de segurança que custam 9.800.
#'  
#' A mão de obra da envoltória será fornecida por uma empresa de construção 
#' a um preço fixo de 197.000. 
#' 
#' A empresa de construção estima que cada piso deve demorar (7,8,9) semanas
#' para ser concluído, dependendo das condições do tempo. 
#' 
#' Entretanto como esta empresa foi comprada por uma empresa maior, existe 
#' uma chance de 10% que o novo dono não aceite o contrato. 
#' 
#' Neste caso, a solução alternativa é utilizar uma segunda empresa 
#' que apresentou uma cotação de 209.000 para o trabalho.
#' 
#' A estimativa da segunda empresa é de (6,8,11) semanas de duração 
#' para cada piso.
#' 

library(triangle)

calcular.envoltoria <- calcular_envoltoria <- 
  calcularEnvoltoria <- function(numberOfSamples){

    print('Calculo da envoltoria do predio')
    
    # O trabalho na envoltória (paredes, janelas e portas externas) só pode ser 
    # iniciado 3 semanas depois do primeiro piso ter sido terminado. 
    prazo.espera <- rep(times=numberOfSamples, x=3)

    # A estimativa para o custo de material é de (36.000, 37.000, 40.000) por 
    # piso dependendo do projeto final de arquitetura. 
    custo.material.piso1 <- rtriangle(n=numberOfSamples, a=36000, b=40000, c=37000)
    custo.material.piso2 <- rtriangle(n=numberOfSamples, a=36000, b=40000, c=37000)
    custo.material.piso3 <- rtriangle(n=numberOfSamples, a=36000, b=40000, c=37000)
    
    # O piso térreo vai necessitar de portas de segurança que custam 9.800.
    custo.material.piso1 <- custo.material.piso1 + 9800
    
    # A mão de obra da envoltória será fornecida por uma empresa de construção 
    # a um preço fixo de 197.000.
    empresa1.mao.de.obra <- rep(times=numberOfSamples, x=197000)

    # A empresa de construção estima que cada piso deve demorar (7,8,9) semanas
    # para ser concluído, dependendo das condições do tempo.
    empresa1.prazo.piso1 <- rtriangle(n=numberOfSamples, a=7, b=9, c=8)
    empresa1.prazo.piso2 <- rtriangle(n=numberOfSamples, a=7, b=9, c=8)
    empresa1.prazo.piso3 <- rtriangle(n=numberOfSamples, a=7, b=9, c=8)
    empresa1.prazo.piso <- empresa1.prazo.piso1 + 
      empresa1.prazo.piso2 + empresa1.prazo.piso3
    
    # Entretanto como esta empresa foi comprada por uma empresa maior, existe 
    # uma chance de 10% que o novo dono não aceite o contrato. 
    vetor.unitario <- rep(times=numberOfSamples, x=1)
    empresa1 <- rbinom(n=numberOfSamples, 1, 0.5)
    empresa2 <- vetor.unitario - empresa1

    # Neste caso, a solução alternativa é utilizar uma segunda empresa 
    # que apresentou uma cotação de 209.000 para o trabalho.
    empresa2.mao.de.obra <- rep(times=numberOfSamples, x=209000)
    
    # A estimativa da segunda empresa é de (6,8,11) semanas de duração 
    # para cada piso.
    empresa2.prazo.piso1 <- rtriangle(n=numberOfSamples, a=6, b=11, c=8)
    empresa2.prazo.piso2 <- rtriangle(n=numberOfSamples, a=6, b=11, c=8)
    empresa2.prazo.piso3 <- rtriangle(n=numberOfSamples, a=6, b=11, c=8)
    empresa2.prazo.piso <- empresa2.prazo.piso1 + 
      empresa2.prazo.piso2 + empresa2.prazo.piso3
    
    empresa1.mao.de.obra.total <- empresa1.mao.de.obra * empresa1
    empresa2.mao.de.obra.total <- empresa2.mao.de.obra * empresa2
    
    custo.mao.de.obra.piso <- empresa1.mao.de.obra.total + 
      empresa2.mao.de.obra.total
    
    empresa1.prazo.piso.total <- empresa1.prazo.piso * empresa1
    empresa2.prazo.piso.total <- empresa2.prazo.piso * empresa2
    prazo.piso <- empresa1.prazo.piso.total + empresa2.prazo.piso.total
    
    custo.material.piso <- custo.material.piso1 + 
      custo.material.piso2 + custo.material.piso3
    
    prazo <- prazo.piso + prazo.espera
    
    result <- list(custo.material=custo.material.piso,
                   custo.mao.de.obra=custo.mao.de.obra.piso,
                   prazo=prazo)
    
    return (result)
    
}
