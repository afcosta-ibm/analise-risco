#' Lista no. 4 – Risco de custo

#' Grupo 1:
#' Alexandre Filgueiras Costa
#' Cristianna Madeira de Ferran
#' Eduardo Chiote
#' Stella Queiroz

# importa as funcoes necessarias
source("grupo-1/projeto-lista-4/centro-comunitario.R")

# armazena os custos do centro comunitario
custos <- centro.Comunitario()

# plota o histograma dos custos
hist(custos, main="Centro Comunitário", xlab = "custo total em US$ 1000")

# cria o range de percentuais de 5 em 5%
percentuais <- (seq(0,1,by=0.05))

# cria a distribuicao dos custos nas faixas de percentuais
distribuicao <- quantile(custos, probs=percentuais)

# imprime os custos nas faixas de percentuais
print(distribuicao)

# plota o grafico dos custos 
plot(distribuicao, percentuais, type='b', col="blue", main="Centro Comunitário",
     xlab="Custo total em US$ 1000", ylab="Percentis", 
     xlim=c(min(distribuicao), max(distribuicao)))

#define a matriz de distribuicao dos percentuais
matrizDistribuicaoPercentuais = matrix(nrow=length(percentuais), ncol=2)
# define a coluna 1 como sendo os custos 
matrizDistribuicaoPercentuais[,1] = distribuicao
# define a coluna 2 como sendo os percentuais 
matrizDistribuicaoPercentuais[,2] = percentuais

# posicao da matriz do custo da obra em 85% dos casos
matrizDistribuicaoPercentuais[18,1]
# risca a linha auxiliar para mostrar o ponto
abline(v = matrizDistribuicaoPercentuais[18,1], col = "red")
abline(h = matrizDistribuicaoPercentuais[18,2], col = "red")
# marca o ponto
points (matrizDistribuicaoPercentuais[18,1], matrizDistribuicaoPercentuais[18,2], col = "green", cex = 0.8)
# escreve uma gracinha
custoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuais[18,1] * 1000, 4)
texto <- paste("P = 85%, Custo: US$", custoCom85PorCentoArredondado)
text(matrizDistribuicaoPercentuais[18,1], matrizDistribuicaoPercentuais[21,2], texto, cex = 0.7, col = "black")

# pega o valor o orcamento aprovado
orcamentoLiberado <- round(mean(custos) * 1000, 4)

# reserva o valor da contingencia
valorContingencia <- custoCom85PorCentoArredondado - orcamentoLiberado 

