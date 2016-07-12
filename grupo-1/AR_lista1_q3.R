#Usando o TCL, obtenha uma aproximação analítica para a variávela aleatória soma de 12
#VAs que seguem uma distribuição uniforme 0..1. Calcule a média e a variância desta
#distribuição e esboce um gráfico dessa distribuição.

#Sejam X1,...,X12 variáveis uniforme no intervalo [0,1] e Z = X1+...+X12.

X = runif(12,0,1)
Z = sum(X)
media_X = 1/2
media_Z = 12*media_X

variancia_X = 1/12
variancia_Z = 12*variancia_X

#Pelo TCL, Z segue uma distribuição Normal com média media_Z e variância variancia_Z


