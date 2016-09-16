#' Lista no. 7 – Risco de prazo com correlacao

#' Grupo 1:
#' Alexandre Filgueiras Costa
#' Cristianna Madeira de Ferran
#' Eduardo Chiote
#' Stella Queiroz

# importa as libs necessarias
library(triangle)
library(igraph)
require("XLConnect")

# importa 
source("grupo-1/projeto-lista-7/projeto.construcao.software.R")
source("grupo-1/projeto-lista-7/gera.grafo.R")
source("grupo-1/projeto-lista-7/excel.matrix.to.xls.R")

# recupera os cenarios de todos os prazos e os sorteios dos eventos
matrizCenarios <- projetoConstrucaoSoftware(10)

# recupera todos os caminhos do grafo
caminhos <- geraGrafo()

matrizCaminhos <- matrix(data = caminhos, 
                         nrow = length(caminhos), 
                         dimnames = list(1:length(caminhos), c("CAMINHOS DO GRAFO")))

# generate a formatted date-time to append on the file path
date.time <- format(Sys.time(), "%Y-%m-%d-%H-%M")

# define a file path to save the spreadsheet
nomeArquivoXLS <- paste("grupo-1/projeto-lista-7/output/lista-7-", date.time, ".xls", sep = "")
nomeImagemGrafo <- "grupo-1/projeto-lista-7/plot-grafo.png"

print(nomeArquivoXLS)

excelMatrixToXls(matrizCenarios = matrizCenarios,
                nomeArquivoImagemGrafo = nomeImagemGrafo,
                caminhosGrafo = matrizCaminhos, nomeArquivo = nomeArquivoXLS)
