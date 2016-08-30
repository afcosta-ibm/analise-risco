#' Lista no. 6 – Risco de custo e prazo

#' Grupo 1:
#' Alexandre Filgueiras Costa
#' Cristianna Madeira de Ferran
#' Eduardo Chiote
#' Stella Queiroz

# importa as funcoes necessarias
source("grupo-1/projeto-lista-6/predio-custo-prazo.R")

# recupera os tempos de todas as atividades de construcao do predio no sitio 
# arqueologico mais o conjunto de tempos do prazo total (todas as atividades)
#todosOsTempos <- predio.Sitio.Arqueologico()

# recupera o conjunto de tempos do prazo total (todas as atividades)
#tempos <- todosOsTempos[["prazoTotal"]]

# plota o histograma dos tempos
#hist(tempos, main="Prédio no Sítio Arqueologico", xlab = "Duração em meses")

# cria o range de percentuais de 5 em 5%
#percentuais <- (seq(0,1,by=0.05))

# cria a distribuicao dos tempos nas faixas de percentuais
#distribuicao <- quantile(tempos, probs=percentuais)

# imprime os tempos nas faixas de percentuais
#print(distribuicao)

# pega o valor o orcamento aprovado
#prazoMedio <- round(mean(tempos), 2)

#textoPrazoMedio <- paste("Duração em meses - Prazo Médio:", prazoMedio)

# plota o grafico dos tempos 
#plot(distribuicao, percentuais, type='l', col="blue", 
#     main="Prédio no Sítio Arqueologico",
#     xlab=textoPrazoMedio, ylab="Percentis", 
#     xlim=c(min(distribuicao), max(distribuicao)))

#define a matriz de distribuicao dos percentuais
#matrizDistribuicaoPercentuais = matrix(nrow=length(percentuais), ncol=2)
# define a coluna 1 como sendo os tempos 
#matrizDistribuicaoPercentuais[,1] = distribuicao
# define a coluna 2 como sendo os percentuais 
#matrizDistribuicaoPercentuais[,2] = percentuais

# posicao da matriz do prazo da obra em 85% dos casos
#matrizDistribuicaoPercentuais[18,1]

# risca a linha auxiliar para mostrar o ponto em 85%
#abline(v = matrizDistribuicaoPercentuais[18,1], col = "green")
#abline(h = matrizDistribuicaoPercentuais[18,2], col = "green")
# marca o ponto
#points (matrizDistribuicaoPercentuais[18,1], matrizDistribuicaoPercentuais[18,2], col = "green", cex = 0.8)
# escreve uma gracinha
#prazoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuais[18,1], 2)
#texto <- paste("P=85%, Prazo:", prazoCom85PorCentoArredondado, '(meses)')
#text(matrizDistribuicaoPercentuais[18,1], matrizDistribuicaoPercentuais[21,2], texto, cex = 0.7, col = "black")

# risca a linha auxiliar para mostrar o ponto em 70%
#abline(v = matrizDistribuicaoPercentuais[15,1], col = "red")
#abline(h = matrizDistribuicaoPercentuais[15,2], col = "red")
# marca o ponto de 70%
#points (matrizDistribuicaoPercentuais[15,1], matrizDistribuicaoPercentuais[15,2], col = "green", cex = 0.8)
# escreve uma gracinha
#prazoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuais[15,1], 2)
#texto <- paste("P=70%, Prazo:", prazoCom85PorCentoArredondado, '(meses)')
#text(matrizDistribuicaoPercentuais[15,1], matrizDistribuicaoPercentuais[12,2], texto, cex = 0.7, col = "black")

# print(' Análise dos maiores prazos de todas as atividades ')
# print(max(todosOsTempos[["arqueologia"]]))
# print(max(todosOsTempos[["escavacao"]]))
# print(max(todosOsTempos[["formas"]]))
# print(max(todosOsTempos[["fundacao"]]))
# print(max(todosOsTempos[["paredes"]]))
# print(max(todosOsTempos[["telhado"]]))

#pontosMaximos <- c(max(todosOsTempos[["arqueologia"]]), 
#                   max(todosOsTempos[["escavacao"]]),
#                   max(todosOsTempos[["formas"]]),
#                   max(todosOsTempos[["fundacao"]]),
#                   max(todosOsTempos[["paredes"]]),
#                   max(todosOsTempos[["telhado"]]))

#legendaAtividades <- "Atividades: 1-Arqueologia, 2 - Escavação, 3-Formas, 4-Fundação, 5-Paredes e 6-Telhado"

#plot(x = pontosMaximos, type = "p", main = "Durações Máximas das Atividades",
#     xlab = legendaAtividades, ylab = "Duração em Meses")
