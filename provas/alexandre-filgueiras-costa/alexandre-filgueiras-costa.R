#' Prova de Análise de Risco - parte 2 - 7 pontos 
#' Aluno: Alexandre Filgueiras Costa
#' 
#' Considerações do projeto.
#' 
#' Atividades (duração) | (custo):
#' A - rtriangle(amostras, 1, 4, 2) | rtriangle(amostras, 75, 110, 90)
#' B - rtriangle(amostras, 5, 7, 6) | rtriangle(amostras, 120, 190, 150)
#' C - rtriangle(amostras, 2, 5, 4) | rtriangle(amostras, 60, 85, 75)
#' D - rtriangle(amostras, 1, 4, 3) | rtriangle(amostras, 45, 80, 60)
#' E - rtriangle(amostras, 4, 7, 5) | rtriangle(amostras, 160, 240, 200)
#' F - rtriangle(amostras, 3, 5, 4) | rtriangle(amostras, 210, 300, 250)
#' G - rtriangle(amostras, 1, 3, 2) | rtriangle(amostras, 85, 140, 120)
#' 
#' - Atividade "F" com 0.25 de ser executada
#' 
#' - Coeficiente de correlação entre as atividades:
#'  - "E" e "F" = 0.70
#'  - "B" e "D" = 0.40
#' 

library(triangle)
library (MASS)
library(igraph)

# define o numero de amostras
NumberOfSamples <- 3000
isDebugMode <- FALSE

# gera o grafo das atividades
G <- make_graph( c( 1,2 ,1,3, 1,4, 2,5, 2,7, 3,8, 4,6, 5,8, 6,8, 7,9, 8,9 )
                 , directed = T)
# plota o grafo
if(isDebugMode){
  plot(G)
}

# lista todos os caminhos do grafo
caminhosGrafo = all_simple_paths(G,from=1,to=9)

if(isDebugMode){
  # imprime os caminhos
  print(caminhosGrafo)
}

# matriz das correlacoes
C = matrix (data = c(1,0.4,0,0,  0.4,1,0,0,  0,0,1,0.7,  0,0,0.7,1), ncol=4)

# funcao para calcular os cenarios de duracao
matriz.Cenarios.Duracao <- function(amostras = 3000){
  
  # monta as colunas da matriz com os nomes corretos
  colunasMatrizDuracaoAtividades <- c("INICIO-1", "A-2", "B-3", "C-4", 
                               "D-5", "E-6", "F-7" ,"G-8", "FIM-9")
  
  # inicializa a matriz de duracao
  matrizCenariosDuracao <- matrix(nrow=amostras, ncol=9,
                           dimnames = list(1:amostras, 
                                           colunasMatrizDuracaoAtividades))

  #' 1 - INICIO - 0
  #' 2 - A - rtriangle(amostras, 1, 4, 2)
  #' 3 - B - rtriangle(amostras, 5, 7, 6)
  #' 4 - C - rtriangle(amostras, 2, 5, 4)
  #' 5 - D - rtriangle(amostras, 1, 4, 3)
  #' 6 - E - rtriangle(amostras, 4, 7, 5)
  #' 7 - F - rtriangle(amostras, 3, 5, 4)
  #' 8 - G - rtriangle(amostras, 1, 3, 2)
  #' 9 - FIM - 0
  
  matrizCenariosDuracao[,1] <- 0
  matrizCenariosDuracao[,2] <- rtriangle(amostras, 1, 4, 2)
  matrizCenariosDuracao[,3] <- rtriangle(amostras, 5, 7, 6)
  matrizCenariosDuracao[,4] <- rtriangle(amostras, 2, 5, 4)
  matrizCenariosDuracao[,5] <- rtriangle(amostras, 1, 4, 3)
  matrizCenariosDuracao[,6] <- rtriangle(amostras, 4, 7, 5)
  matrizCenariosDuracao[,7] <- rtriangle(amostras, 3, 5, 4)
  matrizCenariosDuracao[,8] <- rtriangle(amostras, 1, 3, 2)
  matrizCenariosDuracao[,9] <- 0
  
  return (matrizCenariosDuracao)
}

# funcao para calcular os cenarios de custo
matriz.Cenarios.Custo <- function(amostras = 3000){
  
  # monta as colunas da matriz com os nomes corretos
  colunasMatrizCustoAtividades <- c("INICIO-1", "A-2", "B-3", "C-4", 
                                      "D-5", "E-6", "F-7" ,"G-8", "FIM-9")
  
  # inicializa a matriz de custos
  matrizCenariosCusto <- matrix(nrow=amostras, ncol=9,
                                  dimnames = list(1:amostras, 
                                                  colunasMatrizCustoAtividades))
  
  #' 1 - INICIO - 0
  #' 2 - A - rtriangle(amostras, 75, 110, 90)
  #' 3 - B - rtriangle(amostras, 120, 190, 150)
  #' 4 - C - rtriangle(amostras, 60, 85, 75)
  #' 5 - D - rtriangle(amostras, 45, 80, 60)
  #' 6 - E - rtriangle(amostras, 160, 240, 200)
  #' 7 - F - rtriangle(amostras, 210, 300, 250)
  #' 8 - G - rtriangle(amostras, 85, 140, 120)
  #' 9 - FIM - 0
  
  matrizCenariosCusto[,1] <- 0
  matrizCenariosCusto[,2] <- rtriangle(amostras, 75, 110, 90)
  matrizCenariosCusto[,3] <- rtriangle(amostras, 120, 190, 150)
  matrizCenariosCusto[,4] <- rtriangle(amostras, 60, 85, 75)
  matrizCenariosCusto[,5] <- rtriangle(amostras, 45, 80, 60)
  matrizCenariosCusto[,6] <- rtriangle(amostras, 160, 240, 200)
  matrizCenariosCusto[,7] <- rtriangle(amostras, 210, 300, 250)
  matrizCenariosCusto[,8] <- rtriangle(amostras, 85, 140, 120)
  matrizCenariosCusto[,9] <- 0
  
  return (matrizCenariosCusto)
}

# funcao para calcular os cenarios de sorteio dos eventos
matriz.Cenarios.Eventos <- function(amostras = 3000){
  
  # monta as colunas da matriz com os nomes corretos
  colunasMatrizEventoAtividadeF <- c("SORTEIO")
  
  # inicializa a matriz de eventos
  matrizCenariosEvento <- matrix(nrow=amostras, ncol=1,
                                dimnames = list(1:amostras, 
                                                colunasMatrizEventoAtividadeF))
  
  matrizCenariosEvento[,1] <- sample(x=c(0,1), size=amostras, 
                                     replace=TRUE, prob=c(0.7,0.3))
  
  return (matrizCenariosEvento)
}

matrizPrazo <- matriz.Cenarios.Duracao(NumberOfSamples)
matrizCusto <- matriz.Cenarios.Custo(NumberOfSamples)
matrizEvento <- matriz.Cenarios.Eventos(NumberOfSamples)

# recupera o numero de caminhos possiveis para percorrer o grafo
nroCaminhosGrafo <- length(caminhosGrafo)

if(isDebugMode){
  print(nroCaminhosGrafo)
}

#================== CUSTOS ============================
vetorCustos <- vector(length = NumberOfSamples)

for (i in 1:NumberOfSamples) {
  # variavel temporaria de maximo custo
  maxCusto <- 0
  for(j in 1:nroCaminhosGrafo){
    d <- matrizCusto[i,]
    # soma os custos do caminho
    custoCaminho <- sum(d[caminhosGrafo[[j]]])
    # atualiza o maior custo
    if(custoCaminho > maxCusto){
      maxCusto <- custoCaminho
    }
  }
  vetorCustos[i] <- maxCusto
}

# plota o histograma dos custos do projeto
hist(vetorCustos, main="Custos do Projeto", xlab = "Custo em $ 1000", ylab = "Frequência")
# pega o custo medio do projeto
custoMedio <- round(mean(vetorCustos), 2)
# incfrementa o texto do grafico
textoCustoMedio <- paste("Custo Médio do Projeto: ", custoMedio)

# gera os percentuais de distribuicao
percentuais = (seq(0, 1, by=0.01))

distribuicao = quantile (vetorCustos, probs=percentuais)

plot(distribuicao, percentuais, type = 'l', col = "red", 
     main = textoCustoMedio, xlab = "Custo em $1000", ylab = "percentil",
     xlim = c(min(distribuicao), max(distribuicao)))

matrizDistribuicaoPercentuais = matrix(nrow = length(percentuais), ncol = 2 )
matrizDistribuicaoPercentuais[,1] = distribuicao
matrizDistribuicaoPercentuais[,2] = percentuais

# escreve uma gracinha
custoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuais[86,1], 2)
texto85 <- paste("P=85%, Custo:", custoCom85PorCentoArredondado, '($1000)')

custoCom70PorCentoArredondado <- round(matrizDistribuicaoPercentuais[71,1], 2)
texto70 <- paste("P=70%, Custo:", custoCom70PorCentoArredondado, '($1000)')

#Riscos de custo: d[86,1] (85%) e d[71,1] (70%)
matrizDistribuicaoPercentuais[86,1]
matrizDistribuicaoPercentuais[71,1]

abline (v = matrizDistribuicaoPercentuais[86,1], col = "gray")
abline (h = matrizDistribuicaoPercentuais[86,2], col = "gray")
abline (v = matrizDistribuicaoPercentuais[71,1], col = "gray")
abline (h = matrizDistribuicaoPercentuais[71,2], col = "gray")
points (matrizDistribuicaoPercentuais[86,1], matrizDistribuicaoPercentuais[86,2], col = "green", cex = 0.9)
points (matrizDistribuicaoPercentuais[71,1], matrizDistribuicaoPercentuais[71,2], col = "green", cex = 0.9)
text (matrizDistribuicaoPercentuais[86,1], matrizDistribuicaoPercentuais[90,2], texto85, cex = 0.8, col = "blue")
text (matrizDistribuicaoPercentuais[71,1], matrizDistribuicaoPercentuais[75,2], texto70, cex = 0.8, col = "blue")

#================== FIM CUSTOS ============================

#================== PRAZOS ============================
vetorPrazos <- vector(length = NumberOfSamples)

for (i in 1:NumberOfSamples) {
  # variavel temporaria de maximo prazo
  maxPrazo <- 0
  for(j in 1:nroCaminhosGrafo){
    d <- matrizPrazo[i,]
    # soma os prazos do caminho
    prazoCaminho <- sum(d[caminhosGrafo[[j]]])
    # atualiza o maior prazo
    if(prazoCaminho > maxPrazo){
      maxPrazo <- prazoCaminho
    }
  }
  vetorPrazos[i] <- maxPrazo
}

# plota o histograma dos prazos do projeto
hist(vetorPrazos, main="Prazos do Projeto", xlab = "Prazo em semanas", ylab = "Frequência")
# pega o prazo medio do projeto
prazoMedio <- round(mean(vetorPrazos), 2)
# incrementa o texto do grafico
textoPrazoMedio <- paste("Prazo Médio do Projeto: ", prazoMedio)

# gera os percentuais de distribuicao
percentuaisPrazos = (seq(0, 1, by=0.01))

distribuicaoPrazos = quantile (vetorPrazos, probs=percentuaisPrazos)

plot(distribuicaoPrazos, percentuaisPrazos, type = 'l', col = "red", 
     main = textoPrazoMedio, xlab = "Prazo em semanas", ylab = "percentil",
     xlim = c(min(distribuicaoPrazos), max(distribuicaoPrazos)))

matrizDistribuicaoPercentuaisPrazos = matrix(nrow = length(percentuaisPrazos), ncol = 2 )
matrizDistribuicaoPercentuaisPrazos[,1] = distribuicaoPrazos
matrizDistribuicaoPercentuaisPrazos[,2] = percentuaisPrazos

# escreve uma gracinha
prazoCom85PorCentoArredondado <- round(matrizDistribuicaoPercentuaisPrazos[86,1], 2)
texto85Prazo <- paste("P=85%, Prazo:", prazoCom85PorCentoArredondado, '(semanas)')

prazoCom70PorCentoArredondado <- round(matrizDistribuicaoPercentuaisPrazos[71,1], 2)
texto70Prazo <- paste("P=70%, Prazo:", prazoCom70PorCentoArredondado, '(semanas)')

#Riscos de prazo: d[86,1] (85%) e d[71,1] (70%)
matrizDistribuicaoPercentuaisPrazos[86,1]
matrizDistribuicaoPercentuaisPrazos[71,1]

abline (v = matrizDistribuicaoPercentuaisPrazos[86,1], col = "gray")
abline (h = matrizDistribuicaoPercentuaisPrazos[86,2], col = "gray")
abline (v = matrizDistribuicaoPercentuaisPrazos[71,1], col = "gray")
abline (h = matrizDistribuicaoPercentuaisPrazos[71,2], col = "gray")
points (matrizDistribuicaoPercentuaisPrazos[86,1], matrizDistribuicaoPercentuaisPrazos[86,2], col = "green", cex = 0.9)
points (matrizDistribuicaoPercentuaisPrazos[71,1], matrizDistribuicaoPercentuaisPrazos[71,2], col = "green", cex = 0.9)
text (matrizDistribuicaoPercentuaisPrazos[86,1], matrizDistribuicaoPercentuaisPrazos[90,2], texto85Prazo, cex = 0.8, col = "blue")
text (matrizDistribuicaoPercentuaisPrazos[71,1], matrizDistribuicaoPercentuaisPrazos[75,2], texto70Prazo, cex = 0.8, col = "blue")

#================== FIM PRAZOS ============================


#================== correlacao ============================
# correlacao custo prazo
c <- cor(vetorPrazos, vetorCustos, method="spearman")
print(c)

#normalization
c<-c/sum(c)
m<-matrix(c(1:9,c),ncol=2)
#preparando tornado plot
o<-order(m[,2])
yname<-m[,1][o]
barplot(m[,2][o],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
        names.arg=yname, main = "Correlação",
        xlab="",ylab="Custo")
