# Um casco de navio consiste de 562 placas metálicas que devem ser rebitadas. 
# Estima-se que o tempo gasto por um rebitador seja (3h45,5h30,4h15). 
# O rebitador recebe USD$7.50 por hora trabalhada.
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

#hist(tempoTotalCasco)

#hist(vetorCusto)

forcaBruta <- rnorm(3000, mediaCusto, varianciaCusto^1/2)

#plot(ecdf(forcaBruta))

# pelo TCL
# horas * nro-placas 
# media rebitagem de uma chapa
percentuais <- seq(from = 0, to = 1, by= 0.1)

mediaTempoChapa <- (3.75 + 5.5 + 4.25) / 3
mediaCustoChapa <- mediaTempoChapa * 7.5
mediaCustoCasco <- 562 * mediaCustoChapa

varianciaChapa <- (3.75^2 + 5.5^2 + 4.25^2 - 3.75*5.5 - 3.75*4.24 - 5.5*4.25) / 18

varianciaCusto <- varianciaChapa * 7.5 ^ 2

desvioPadraoCasco <- varianciaCusto * 0.5

print(percentuais)
print(mediaTempoChapa)
print(varianciaChapa)
print(varianciaCusto)
print(mediaCustoCasco)
print(desvioPadraoCasco)


yTCL <- qnorm(percentuais, mean=18967, sd = 65.4)


# custoRebitagemCasco <- ( (225 + 255 + 330) /3 ) * 562
# x<-rnorm(3000, 19000, 200)
# 
# yTCL <- quantile(x, percentuais)
# 
