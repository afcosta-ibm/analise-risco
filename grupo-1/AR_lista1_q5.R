#Usando o TCL, obtenha uma aproximação analítica para a soma de n (n= 2,5,10) distribuições 
#triangulares. Suponha que a distribuição i tem como parâmetros (i-1,i,i+1). 
#Calcule a média e a variância plote um gráfico dessas distribuições.

library (triangle)


#para n = 2

n = 2
X = NULL
for (i in 1:n){
  
  pmin = i-1
  pmax = i+1
  mprov = i
  
  X[i] = rtriangle(n, pmin, pmax,mprov)

}

Z = sum(X)

media1 = mean(X)
variancia1 = var (X)
plot(X)

#para n = 5

n = 5
X = NULL
for (i in 1:n){
  
  pmin = i-1
  pmax = i+1
  mprov = i
  
  X[i] = rtriangle(n, pmin, pmax,mprov)

}

  Z = sum(X)
  
  media2 = mean(X)
  variancia2 = var (X)
  plot(X)
  
  #para n = 10
  
  n = 10
  X = NULL
  for (i in 1:n){
    
    pmin = i-1
    pmax = i+1
    mprov = i
    
    X[i] = rtriangle(n, pmin, pmax,mprov)
    
  }
  
  Z = sum(X)
  
  media3 = mean(X)
  variancia3 = var (X)
  plot(X)  