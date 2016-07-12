#Usando simulação, obtenha uma aproximação empírica para o produto de duas VAs
#com distribuição normal com média =0 e variância =1.



X = NULL
Y = NULL
Z = NULL
for (i in 1:1000) {
 
    X[i] = rnorm(1)
    Y[i] = rnorm(1)
    Z[i] = X[i] * Y[i]
    
}

media3 = mean(Z)
variancia3 = var (Z)
hist(Z)