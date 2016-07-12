#Usando simulação, obtenha uma aproximação empírica para a distribuição de probabilidade
#da soma de 12 VAs uniformes 0..1.


#Sejam X1,...,X12 variáveis uniforme no intervalo [0,1] e Z = X1+...+X12.
y = 1:1000

for (i in 1:1000) {
 X = runif(12,0,1)
 Z[i] = sum(X)
}

hist(Z)
