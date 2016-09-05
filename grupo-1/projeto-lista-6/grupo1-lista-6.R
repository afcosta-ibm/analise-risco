#' Lista no. 6 – Risco de custo e prazo

#' Grupo 1:
#' Alexandre Filgueiras Costa
#' Cristianna Madeira de Ferran
#' Eduardo Chiote
#' Stella Queiroz

# importa as funcoes necessarias
source("grupo-1/projeto-lista-6/predio-secao-1.R")
source("grupo-1/projeto-lista-6/predio-custo-prazo.R")

secao1 <- predioSecao1()

secao1.prazos <- secao1[["prazos"]]
secao1.custos <- secao1[["custos"]]

# plota o histograma dos prazos da secao 1
hist(secao1.prazos, main="Seção 1 Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")

# plota o histograma dos prazos da secao 1
hist(secao1.custos, main="Seção 1 Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")

