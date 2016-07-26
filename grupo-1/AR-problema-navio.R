# Um casco de navio consiste de 562 placas metálicas que devem ser rebitadas. 
# Estima-se que o tempo gasto por um rebitadorseja (3h45,5h30,4h15). 
# O rebitadorrecebe USD$7.50 por hora trabalhada.
# Qual o risco de custo de mão-obra para a rebitagem?
# Compare as distribuições cumulativas obtidas usando a abordagem força-bruta 
# com aquela usando o TCL.

library(triangle)

vetorCusto <- c()
tempoTotalCasco <- c()

for (i in 1:3000) {
  
  # convertemos hora para minutos
  tempoCasco <- rtriangle(562, 225/60, 330/60, 255/60)
  tempoTotalCasco[i] <- sum(tempoCasco)
  
  valorCasco <- sum(tempoCasco) * 7.5
  
  vetorCusto[i] <- valorCasco
}

mediaCusto <- mean(vetorCusto)
varianciaCusto <- var(vetorCusto)

hist(tempoTotalCasco)

hist(vetorCusto)

forcaBruta <- rnorm(3000, mediaCusto, varianciaCusto^1/2)

plot(ecdf(forcaBruta))

