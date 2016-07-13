# Usando o TCL, obtenha uma aproximacao analitica para a soma de n (n = 2,5,10) 
# distribuicoes triangulares. 
# Suponha que a distribuicao i tem como parametros (i-1,i,i+1). 
# Calcule a media e a variancia plote um grafico dessas distribuicoes.

library (triangle)

#para n <- 2

n <- 2
X <- NULL

for (i in 1:n){
  pmin <- i-1
  mprov <- i
  pmax <- i+1

  X[i] <- rtriangle(n, pmin, pmax, mprov)
}

Z <- sum(X)

media1 <- mean(X)
variancia1 <- var (X)
plot(X, main = "N = 2")

#para n <- 5

n <- 5
X <- NULL
for (i in 1:n){
  
  pmin <- i-1
  pmax <- i+1
  mprov <- i
  
  X[i] <- rtriangle(n, pmin, pmax,mprov)

}

Z <- sum(X)

media2 <- mean(X)
variancia2 <- var (X)
plot(X, main = "N = 5")

#para n <- 10

n <- 10
X <- NULL
for (i in 1:n){
  
  pmin <- i-1
  pmax <- i+1
  mprov <- i
  
  X[i] <- rtriangle(n, pmin, pmax,mprov)
  
}

Z <- sum(X)

media3 <- mean(X)
variancia3 <- var (X)
plot(X, main = "N = 10")
