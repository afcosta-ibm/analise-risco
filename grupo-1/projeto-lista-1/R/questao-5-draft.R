#Usando o TCL, obtenha uma aproxima??o anal?tica para a soma de n (n<- 2,5,10) distribui??es 
#triangulares. Suponha que a distribui??o i tem como par?metros (i-1,i,i+1). 
#Calcule a m?dia e a vari?ncia plote um gr?fico dessas distribui??es.

library (triangle)

#para n <- 5


n <- 5
Z <- c(rep(0,n))

for (i in 1:n) {
  
  pmin <- i-1
  pmax <- i+1
  mprov <- i
  
  Z <- Z + rtriangle(n, pmin, pmax, mprov)

}


# Calcule a média e a variância plote um gráfico dessas distribuições.
media <- mean(Z)
variancia <- var (Z)
histogram <- hist(Z)
xfit <- seq(min(Z), max(Z), length = 100)
yfit <- dnorm(xfit, mean=mean(Z), sd=sd(Z))
yfit <- yfit * diff(histogram$mids[1:2])*length(Z)
lines(xfit, yfit, col="black", lwd=2)
  
 