# Usando o TCL, obtenha uma aproximacao analatica para a variavela aleatoria 
# soma de 12 VAs que seguem uma distribuicao uniforme 0..1. 
# Calcule a media e a variancia desta distribuicao e esboce um grafico 
# dessa distribuicao. 

#Sejam X1,...,X12 variaveis uniformes no intervalo [0,1] e Z <- X1+...+X12.

X <- runif(12,0,1)
Z <- sum(X)
media_X <- 1/2
media_Z <- 12*media_X

variancia_X <- 1/12
variancia_Z <- 12*variancia_X

hist(X)

# Pelo TCL, Z segue uma distribuicao Normal
# com media media_Z e variancia variancia_Z