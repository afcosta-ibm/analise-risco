#Usando simula??o, obtenha uma aproxima??o emp?rica para a distribui??o de probabilidade
#da soma de 12 VAs uniformes 0..1.


#Sejam X1,...,X12 vari?veis uniforme no intervalo [0,1] e Z = X1+...+X12.

for (i in 1:1000) {
 X <- runif(12,0,1)
 Z[i] <- sum(X)
}

hist(Z)
