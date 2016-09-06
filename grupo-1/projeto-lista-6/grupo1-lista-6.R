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
     
# cria o range de percentuais de 5 em 5%
percentuais <- (seq(0,1,by=0.05))

# cria a distribuicao dos tempos nas faixas de percentuais
distribuicao <- quantile(secao1.prazos, probs=percentuais)

# imprime os tempos nas faixas de percentuais
print(distribuicao)

# pega o valor o orcamento aprovado
prazoMedio <- round(mean(secao1.prazos), 2)

textoPrazoMedio <- paste("Duração em meses - Prazo Médio:", prazoMedio)

# plota o grafico dos tempos 
plot(distribuicao, percentuais, type='l', col="blue", 
     main="Prédio",
     xlab=textoPrazoMedio, ylab="Percentis", 
     xlim=c(min(distribuicao), max(distribuicao)))

#define a matriz de distribuicao dos percentuais
matrizDistribuicaoPercentuais = matrix(nrow=length(percentuais), ncol=2)
# define a coluna 1 como sendo os tempos 
matrizDistribuicaoPercentuais[,1] = distribuicao
# define a coluna 2 como sendo os percentuais 
matrizDistribuicaoPercentuais[,2] = percentuais

# posicao da matriz do prazo da obra em 85% dos casos
matrizDistribuicaoPercentuais[18,1]

# risca a linha auxiliar para mostrar o ponto em 85%
abline(v = matrizDistribuicaoPercentuais[18,1], col = "green")
abline(h = matrizDistribuicaoPercentuais[18,2], col = "green")
# marca o ponto
points (matrizDistribuicaoPercentuais[18,1], matrizDistribuicaoPercentuais[18,2], col = "green", cex = 0.8)
# escreve uma gracinha
prazoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuais[18,1], 2)
texto <- paste("P=85%, Prazo:", prazoCom85PorCentoArredondado, '(meses)')
text(matrizDistribuicaoPercentuais[18,1], matrizDistribuicaoPercentuais[21,2], texto, cex = 0.7, col = "black")

# risca a linha auxiliar para mostrar o ponto em 70%
abline(v = matrizDistribuicaoPercentuais[15,1], col = "red")
abline(h = matrizDistribuicaoPercentuais[15,2], col = "red")
# marca o ponto de 70%
points (matrizDistribuicaoPercentuais[15,1], matrizDistribuicaoPercentuais[15,2], col = "green", cex = 0.8)
# escreve uma gracinha
prazoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuais[15,1], 2)
texto <- paste("P=70%, Prazo:", prazoCom85PorCentoArredondado, '(meses)')
text(matrizDistribuicaoPercentuais[15,1], matrizDistribuicaoPercentuais[12,2], texto, cex = 0.7, col = "black")

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
