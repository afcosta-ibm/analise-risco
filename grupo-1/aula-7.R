#' Exercício 1
#' Gere amostras de tamanho 1000 de uma normal (0,1). Construa um gráfico 
#' mostrando a convergência da média e da variância com o tamanho da amostra.

NumberOfSamples <- 1000

# vetor com as amostras
x <- rnorm(NumberOfSamples)

# vetor com as medias
vm <- vector(length = NumberOfSamples)


for (i in 1:NumberOfSamples) {
  vm[i] <- sd(x[1:i])
}

plot(vm, type = "l")

#' Como gerar a ecdf(x):
#' 1 - gere x[1:N] amostras de uma VA
#' 2 - ordene os valores de x (crescente)
#' 3 - construa um vetor y tqy[i] = i/N
#' 4 - y é a ecdf(x)

# vetor com as amostras
x <- rnorm(NumberOfSamples)

xOrd <- sort(x)
ecds <- 1:NumberOfSamples/NumberOfSamples

plot(xOrd, ecds, type = "l")

