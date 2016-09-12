#' Lista no. 6 – Risco de custo e prazo

#' Grupo 1:
#' Alexandre Filgueiras Costa
#' Cristianna Madeira de Ferran
#' Eduardo Chiote
#' Stella Queiroz

# importa as funcoes necessarias
source("grupo-1/projeto-lista-6/predio-custo-prazo.R")

#'-------------------------------------------------------------------------

matrizCenarios <- predioCustoPrazo(3000)

# plota o histograma dos prazos da secao 1
hist(matrizCenarios["projeto.prazos",], main="Seção 1 Prazos - Prédio", 
     xlab = "Duração em meses",
     ylab = "Frequência")

# plota o histograma dos prazos da secao 1
hist(matrizCenarios["projeto.custos",], main="Seção 1 Custos - Prédio", 
     xlab = "Custo em US$",
     ylab = "Frequência")

# cria o range de percentuais de 5 em 5%
percentuais <- (seq(0,1,by=0.05))

# cria a distribuicao dos tempos nas faixas de percentuais
distribuicao <- quantile(matrizCenarios["projeto.prazos",], probs=percentuais)

# imprime os tempos nas faixas de percentuais
# print(distribuicao)

# pega o valor o orcamento aprovado
prazoMedio <- round(mean(matrizCenarios["projeto.prazos",]), 2)

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
points (matrizDistribuicaoPercentuais[18,1], 
        matrizDistribuicaoPercentuais[18,2], col = "green", cex = 0.8)
# escreve uma gracinha
prazoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuais[18,1], 2)
texto <- paste("P=85%, Prazo:", prazoCom85PorCentoArredondado, '(meses)')
text(matrizDistribuicaoPercentuais[18,1], matrizDistribuicaoPercentuais[21,2], 
     texto, cex = 0.7, col = "black")

# risca a linha auxiliar para mostrar o ponto em 70%
abline(v = matrizDistribuicaoPercentuais[15,1], col = "red")
abline(h = matrizDistribuicaoPercentuais[15,2], col = "red")
# marca o ponto de 70%
points (matrizDistribuicaoPercentuais[15,1], 
        matrizDistribuicaoPercentuais[15,2], col = "green", cex = 0.8)

# escreve uma gracinha
prazoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuais[15,1], 2)
texto <- paste("P=70%, Prazo:", prazoCom85PorCentoArredondado, '(meses)')
text(matrizDistribuicaoPercentuais[15,1], matrizDistribuicaoPercentuais[12,2], 
     texto, cex = 0.7, col = "black")

#'-------------------------------------------------------------------------

# plota o histograma dos prazos da secao 2
hist(matrizCenarios["terraplanagem.prazos",], main="Seção 2 Prazos - Prédio", 
     xlab = "Duração em meses",
     ylab = "Frequência")

# plota o histograma dos prazos da secao 2
hist(matrizCenarios["terraplanagem.custos",], main="Seção 2 Custos - Prédio", 
     xlab = "Custo em US$",
     ylab = "Frequência")

#'-------------------------------------------------------------------------

# plota o histograma dos prazos da secao 3
hist(matrizCenarios["fundacoes.prazos",], main="Seção 3 Prazos - Prédio", 
     xlab = "Duração em meses",
     ylab = "Frequência")

# plota o histograma dos prazos da secao 3
hist(matrizCenarios["fundacoes.custos",], main="Seção 3 Custos - Prédio", 
     xlab = "Custo em US$",
     ylab = "Frequência")

#'-------------------------------------------------------------------------
# calculo da estrutura
hist(matrizCenarios["estrutura.prazos",], 
     main="Estrutura - Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")
hist(matrizCenarios["estrutura.custos",], 
     main="Estrutura - Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")
#'-------------------------------------------------------------------------
# calculo da envoltoria
hist(matrizCenarios["envoltoria.prazos",], 
     main="Envoltoria - Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")
hist(matrizCenarios["envoltoria.custos",], 
     main="Envoltoria - Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")
#'-------------------------------------------------------------------------
# calculo do servico de acabamento
hist(matrizCenarios["servico.acabamento.prazos",], 
     main="Servico Acabamento - Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")
hist(matrizCenarios["servico.acabamento.custos",], 
     main="Servico Acabamento - Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")
#'-------------------------------------------------------------------------
# calculo da finalizacao
hist(matrizCenarios["finalizacao.prazos",], 
     main="Finalizacao - Prazos - Prédio", xlab = "Duração em meses",
     ylab = "Frequência")
hist(matrizCenarios["finalizacao.custos",], 
     main="Finalizacao - Custos - Prédio", xlab = "Custo em US$",
     ylab = "Frequência")
