#Usando o TCL, obtenha uma aproxima??o anal?tica para a vari?vela aleat?ria soma de 12
#VAs que seguem uma distribui??o uniforme 0..1. Calcule a m?dia e a vari?ncia desta
#distribui??o e esboce um gr?fico dessa distribui??o.

#Sejam X1,...,X12 vari?veis uniforme no intervalo [0,1] e Z = X1+...+X12.

X = runif(12,0,1)
Z = sum(X)
media_X = 1/2
media_Z = 12*media_X

variancia_X = 1/12
variancia_Z = 12*variancia_X

#Pelo TCL, Z segue uma distribui??o Normal com m?dia media_Z e vari?ncia variancia_Z


