#Usando simula��o, obtenha uma aproxima��o emp�rica para a distribui��o de probabilidade
#das VAs do exerc�cio 5.

library (triangle)


#para n = 2

n = 2
X = NULL
Z = NULL
for (j in 1:1000) {
for (i in 1:n){
  
  pmin = i-1
  pmax = i+1
  mprov = i
  
  X[i] = rtriangle(n, pmin, pmax,mprov)
  
}

Z[j] = sum(X)
}
media1 = mean(Z)
variancia1 = var (Z)
hist(Z)

#para n = 5

n = 5
X = NULL
Z = NULL
for (j in 1:1000) {
for (i in 1:n){
  
  pmin = i-1
  pmax = i+1
  mprov = i
  
  X[i] = rtriangle(n, pmin, pmax,mprov)
  
}

  Z[j] = sum(X)
}

media2 = mean(Z)
variancia2 = var (Z)
hist(Z)

#para n = 10

n = 10
X = NULL
Z = NULL
for (j in 1:1000) {
  for (i in 1:n){
    
    pmin = i-1
    pmax = i+1
    mprov = i
    
    X[i] = rtriangle(n, pmin, pmax,mprov)
    
  }
  
  Z[j] = sum(X)
}

media3 = mean(Z)
variancia3 = var (Z)
hist(Z)

#d�vida: an�lise da distribui��o baseando-se somente no histograma?