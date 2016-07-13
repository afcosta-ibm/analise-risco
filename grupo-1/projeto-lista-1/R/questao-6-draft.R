# Usando simulacao, obtenha uma aproximacao empirica para a distribuicao de probabilidade
# das VAs do exercicio 5.

library (triangle)

#para n = 10

n <- 10
X <- NULL
Z <- NULL

for (j in 1:10000) {
  for (i in 1:n) {
  
    pmin <- i-1
    pmax <- i+1
    mprov <- i
    
    X[i] <- rtriangle(n, pmin, pmax,mprov)
    
  }
  
  Z[j] <- sum(X)
}

# Calcule a média e a variância plote um gráfico dessas distribuições.
media <- mean(Z)
variancia <- var (Z)
histogram <- hist(Z)
xfit <- seq(min(Z), max(Z), length = 40)
yfit <- dnorm(xfit, mean=mean(Z), sd=sd(Z))
yfit <- yfit * diff(histogram$mids[1:2])*length(Z)
lines(xfit, yfit, col="black", lwd=2)

#duvida: analise da distribuicao baseando-se somente no histograma?

