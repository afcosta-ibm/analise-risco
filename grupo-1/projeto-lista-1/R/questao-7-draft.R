n <- 10000
pmin <- -1
pmax <- 1
mprov <- 0

Z <- rtriangle(n, pmin, pmax, mprov)

# Calcule a média e a variância plote um gráfico dessas distribuições.
media <- mean(Z)
variancia <- var (Z)
histogram <- hist(Z)
xfit <- seq(min(Z), max(Z), length = 100)
yfit <- dnorm(xfit, mean=mean(Z), sd=sd(Z))
yfit <- yfit * diff(histogram$mids[1:2])*length(Z)
lines(xfit, yfit, col="black", lwd=2)

