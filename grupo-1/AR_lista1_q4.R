# Usando simulacao, obtenha uma aproximacao empirica para a distribuicao de 
# probabilidade da soma de 12 VAs uniformes 0..1.

#Sejam X1,...,X12 variaveis uniforme no intervalo [0,1] e Z = X1+...+X12.
Z <- NULL

for (i in 1:1000) {
<<<<<<< HEAD
  X <- runif(12,0,1)
=======
  X = runif(12,0,1)
>>>>>>> 9418d08c93abc2168c81deeed837aab8d867564f
  Z[i] <- sum(X)
}

h <- hist(Z)
xfit <- seq(min(Z), max(Z), length = 40)
yfit <- dnorm(xfit, mean=mean(Z), sd=sd(Z))
yfit <- yfit * diff(h$mids[1:2])*length(Z)

lines(xfit, yfit, col="black", lwd=2)