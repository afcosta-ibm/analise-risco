#' Lista no. 6 – Risco de custo e prazo

#' Grupo 1:
#' Alexandre Filgueiras Costa
#' Cristianna Madeira de Ferran
#' Eduardo Chiote
#' Stella Queiroz

# importa as funcoes necessarias
source("grupo-1/projeto-lista-6/predio-secao-1.R")
source("grupo-1/projeto-lista-6/predio-secao-2.R")
source("grupo-1/projeto-lista-6/predio-secao-3.R")
source("grupo-1/projeto-lista-6/predio-custo-prazo.R")
source("grupo-1/projeto-lista-6/calcular-estrutura.R")
source("grupo-1/projeto-lista-6/calcular-envoltoria.R")
source("grupo-1/projeto-lista-6/calcular-servico-acabamento.R")
source("grupo-1/projeto-lista-6/calcular-finalizacao.R")

#'-------------------------------------------------------------------------

# calculo da secao 1
secao1 <- predioSecao1()

secao1.prazos <- secao1[["prazos"]]
secao1.custos <- secao1[["custos"]]

# plota o histograma dos prazos da secao 1
hist(secao1.prazos, main="Seção 1 Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")

# plota o histograma dos prazos da secao 1
hist(secao1.custos, main="Seção 1 Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")

#'-------------------------------------------------------------------------

# calculo da secao 2
secao2 <- predioSecao2()

secao2.prazos <- secao2[["prazos"]]
secao2.custos <- secao2[["custos"]]

# plota o histograma dos prazos da secao 2
hist(secao2.prazos, main="Seção 2 Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")

# plota o histograma dos prazos da secao 2
hist(secao2.custos, main="Seção 2 Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")

#'-------------------------------------------------------------------------

# calculo da secao 3
secao3 <- predioSecao3()

secao3.prazos <- secao3[["prazos"]]
secao3.custos <- secao3[["custos"]]

# plota o histograma dos prazos da secao 3
hist(secao3.prazos, main="Seção 3 Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")

# plota o histograma dos prazos da secao 3
hist(secao3.custos, main="Seção 3 Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")

#'-------------------------------------------------------------------------
# calculo da estrutura
estrutura <- calcularEstrutura(numberOfSamples=3000)
hist(estrutura[["prazo"]], main="Estrutura - Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")
hist(estrutura[["custo"]], main="Estrutura - Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")
#'-------------------------------------------------------------------------
# calculo da envoltoria
envoltoria <- calcularEnvoltoria(numberOfSamples=3000)
hist(envoltoria[["prazo"]], main="Envoltoria - Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")
hist(envoltoria[["custo"]], main="Envoltoria - Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")
#'-------------------------------------------------------------------------
# calculo do servico de acabamento
servico.acabamento <- calcularServicoAcabamento(numberOfSamples=3000)
hist(servico.acabamento[["prazo"]], main="Servico Acabamento - Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")
hist(servico.acabamento[["custo"]], main="Servico Acabamento - Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")
#'-------------------------------------------------------------------------
# calculo da finalizacao
finalizacao <- calcularFinalizacao(numberOfSamples=3000)
hist(finalizacao[["prazo"]], main="Finalizacao - Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")
hist(finalizacao[["custo"]], main="Finalizacao - Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")
