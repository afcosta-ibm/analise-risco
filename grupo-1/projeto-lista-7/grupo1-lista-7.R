#' Lista no. 6 – Risco de custo e prazo

#' Grupo 1:
#' Alexandre Filgueiras Costa
#' Cristianna Madeira de Ferran
#' Eduardo Chiote
#' Stella Queiroz

# importa as libs necessarias
library(triangle)
library(igraph)

# importa 
source("grupo-1/projeto-lista-7/projeto.construcao.software.R")
source("grupo-1/projeto-lista-7/gera.grafo.R")

# recupera os cenarios de todos os prazos e os sorteios dos eventos
matrizCenarios <- projetoConstrucaoSoftware(10)

# recupera todos os caminhos do grafo
caminhos <- geraGrafo()