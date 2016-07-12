#Obtenha uma aproximação empírica para a função Máximo( Xi) (i=2,5,10) que representa a distribuição
#de probabilidade do máximo dentre i VAs cada uma delas representando uma Normal(0,1).

#para n = 2

n = 2
Z = NULL

for (i in 1:1000) {
  
    Z[i] = max(rnorm(n))
  
}

media1 = mean(Z)
variancia1 = var (Z)
hist(Z)

#para n = 5

n = 5
Z = NULL

for (i in 1:1000) {
  
  Z[i] = max(rnorm(n))
  
}

media2 = mean(Z)
variancia2 = var (Z)
hist(Z)

#para n = 10

n = 10
Z = NULL

for (i in 1:1000) {
  
  Z[i] = max(rnorm(n))
  
}

media3 = mean(Z)
variancia3 = var (Z)
hist(Z)