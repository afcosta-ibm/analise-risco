#Usando simula??o, obtenha uma aproxima??o emp?rica para o produto de duas VAs
#com distribui??o normal com m?dia <-0 e vari?ncia <-1.



X <- NULL
Y <- NULL
Z <- NULL
for (i in 1:1000) {
 
    X[i] <- rnorm(1)
    Y[i] <- rnorm(1)
    Z[i] <- X[i] * Y[i]
    
}

media3 <- mean(Z)
variancia3 <- var (Z)
hist(Z)