#Teste de Análise de Risco - Parte 2

#NOME: Stella Cipriano Queiroz
#DRE: 115015003

#pacotes requeridos

library(triangle)
library (MASS)
library(igraph)
library(stats)
library (RColorBrewer)

#grafo representante do problema

g = make_graph(c(1,2,1,3,1,4,2,5,2,6,5,8,3,8,4,7,7,8,6,9,8,9),directed=TRUE)
plot(g, main = "Grafo Representante do Projeto")

#gerar todos os caminhos possíveis
path = all_simple_paths(g,from=1,to=9)
print(path)
l = length(path)

#gerar distribuição Triangular Multivariada

tri.multi = function (C, N) {
  
  #gerar Z[m,n], m n-plas amostras normais reduzidas correlacionadas por C:
  
  Z<-mvrnorm(N,mu=rep(0,times=7),Sigma=C)
  
  #gerar U com m n-plas uniformes:
  
  U<-pnorm(Z)
  
  return (U)
}

#Atividade INÍCIO (1)

inicio = function (N){
  duracao_inicio = rep(0,N)
  custo_inicio = rep(0,N)
  return (cbind (duracao_inicio, custo_inicio))
}

#Atividade A (2)

atividade_A = function (U){
  duracao_A = qtriangle (U[,1],1,4,2)
  custo_A = qtriangle (U[,1],75,110,90)
  return (cbind (duracao_A, custo_A))
}

#Atividade B (3)

atividade_B = function (U){
  duracao_B = qtriangle (U[,2],5,7,6)
  custo_B = qtriangle (U[,2],120,190,150)
  return (cbind (duracao_B, custo_B))
}

#Atividade C (4)

atividade_C = function (U){
  duracao_C = qtriangle (U[,3],2,5,4)
  custo_C = qtriangle (U[,3],60,85,75)
  return (cbind (duracao_C, custo_C))
}

#Atividade D (5)

atividade_D = function (U){
  duracao_D = qtriangle (U[,4],1,4,3)
  custo_D = qtriangle (U[,4],45,80,60)
  return (cbind (duracao_D, custo_D))
}

#Atividade E (6)

atividade_E = function (U){
  duracao_E = qtriangle (U[,5],4,7,5)
  custo_E = qtriangle (U[,5],160,240,200)
  return (cbind (duracao_E, custo_E))
}

#Atividade F (7)

atividade_F = function (U,N){
  evento_F = rbinom(N,1,0.25)
  duracao_F = qtriangle (U[,6],3,5,4)
  custo_F = qtriangle (U[,6],210,300,250)
  return (cbind (duracao_F, custo_F, evento_F))
}

#Atividade G (8)

atividade_G = function (U){
  duracao_G = qtriangle (U[,7],1,3,2)
  custo_G = qtriangle (U[,7],85,140,120)
  return (cbind (duracao_G, custo_G))
}

#Atividade FIM (9)

fim = function (N){
  duracao_fim = rep(0,N)
  custo_fim = rep(0,N)
  return (cbind (duracao_fim, custo_fim))
}

#SIMULAÇÃO

N = 3000 #número de observações

cenarios_duracao = matrix(nrow = N, ncol = 10) #matriz de cenários para durações
cenarios_custo = matrix(nrow = N, ncol = 10) # matriz de cenários para custos
duracao_total = NULL
custo_total = NULL

colnames(cenarios_duracao) = c("INICIO", "A", "B", "C", "D", "E", "F", "G", "FIM", "eventoF")
colnames(cenarios_custo) = c("INICIO", "A", "B", "C", "D", "E", "F", "G", "FIM", "eventoF")

#carregando a matriz de correlações (escolher diretório antes de chamar o arquivo)

matrizC=matrix(c(1,0,0,0,0,0,0,
                0,1,0,0.4,0,0,0,
                0,0,1,0,0,0,0,
                0,0.4,0,1,0,0,0,
                0,0,0,0,1,0.7,0,
                0,0,0,0,0.7,1,0,
                0,0,0,0,0,0,1),ncol=7)

colnames(matrizC) <- c("A","B","C","D","E","F","G")  
 

U = tri.multi(matrizC, N)

#durações
cenarios_duracao [,1] = inicio(N)[,1]
cenarios_duracao [,2] = atividade_A(U)[,1]
cenarios_duracao [,3] = atividade_B(U)[,1]
cenarios_duracao [,4] = atividade_C(U)[,1]
cenarios_duracao [,5] = atividade_D(U)[,1]
cenarios_duracao [,6] = atividade_E(U)[,1]
cenarios_duracao [,8] = atividade_G(U)[,1]
cenarios_duracao [,9] = fim(N)[,1]

#durações e custos condicionais da atividade F

F = atividade_F(U,N)
for (i in 1:N) {
  if (F[i,3] == 1) {
cenarios_duracao [i,7] = F[i,1]
cenarios_custo [i,7] = F[i,2]
}
  else {cenarios_duracao [i,7] = 0
        cenarios_custo [i,7] = 0}
  }

#custos
cenarios_custo [,1] = inicio(N)[,2]
cenarios_custo [,2] = atividade_A(U)[,2]
cenarios_custo [,3] = atividade_B(U)[,2]
cenarios_custo [,4] = atividade_C(U)[,2]
cenarios_custo [,5] = atividade_D(U)[,2]
cenarios_custo [,6] = atividade_E(U)[,2]
cenarios_custo [,8] = atividade_G(U)[,2]
cenarios_custo [,9] = fim(N)[,2]

#eventos
cenarios_duracao [,10] = F[,3]
cenarios_custo [,10] = F[,3]

#calcular duração total

for (i in 1:N) {
  #generate activity duration vectors
  d = cenarios_duracao[i,]
  maxd = 0
  for (j in 1:l){
    pd = sum(d[path[[j]]])
    if (pd > maxd) {
      maxd = pd
    }	
  }	
  duracao_total[i] = maxd
}

#calcular custo total

custo_total = rowSums(cenarios_custo)

#conferindo se as correlações estão corretas

#B e D
cor(cenarios_custo[,3], cenarios_custo[,5])
cor(cenarios_duracao[,3], cenarios_duracao[,5])

#E e F
cor(cenarios_custo[,6], F[,2])
cor(cenarios_duracao[,6], F[,1])

#Gráficos de distribuição - Histogramas, médias e desvios-padrão

hist(duracao_total, main = "Histograma da duração total do projeto")
mean(duracao_total)
sd(duracao_total)

hist(custo_total, main = "Histograma do custo total do projeto")
mean(custo_total)
sd(custo_total)

#Gráficos das distribuições cumulativas

#DURAÇÃO

perc = (seq(0, 1, by=0.01))

x = quantile (duracao_total, probs=perc)

plot(x, perc, type = 'l', col = "red", 
     main = "", xlab = "duracao total", ylab = "percentil",
     xlim = c(min(x), max(x)))

d = matrix(nrow = length(perc), ncol = 2 )
d[,1] = x
d[,2] = perc

#Riscos de duracao: d[86,1] (85%) e d[71,1] (70%)

d[86,1]
d[71,1]

abline (v = d[86,1], col = "gray")
abline (h = d[86,2], col = "gray")
abline (v = d[71,1], col = "gray")
abline (h = d[71,2], col = "gray")
points (d[86,1], d[86,2], col = "green", cex = 0.9)
points (d[71,1], d[71,2], col = "green", cex = 0.9)
text (d[86,1], d[90,2], "p:85%, duracao total:8.9", cex = 0.8, col = "blue")
text (d[71,1], d[75,2], "p:70%, duracao total:9.6", cex = 0.8, col = "blue")


#CUSTO

perc = (seq(0, 1, by=0.01))
y = quantile (custo_total, probs = perc)

plot(y, perc, type = 'l', col = "red", 
     main = "", xlab = "custo total", ylab = "percentil",
     xlim = c(min(y), max(y)))

r = matrix(nrow = length(perc), ncol = 2 )
r[,1] = y
r[,2] = perc

#Riscos de duracao: d[86,1] (85%) e d[71,1] (70%)

r[86,1]
r[71,1]

abline (v = r[86,1], col = "gray")
abline (h = r[86,2], col = "gray")
abline (v = r[71,1], col = "gray")
abline (h = r[71,2], col = "gray")
points (r[86,1], r[86,2], col = "green", cex = 0.9)
points (r[71,1], r[71,2], col = "green", cex = 0.9)
text (r[86,1], r[90,2], "p:85%, duracao total:938", cex = 0.8, col = "blue")
text (r[71,1], r[75,2], "p:70%, duracao total:737", cex = 0.8, col = "blue")


#Relação entre custo e prazo - Gráfico

#o gráfico estabelece a diferença entre cenários com e sem evento

relacao = cbind(duracao_total,custo_total)

k=kmeans(relacao,2)

plot(relacao, col = k$cluster, main = "Relação entre custo e prazo")

text (7.3, 730, "sem evento", cex = 0.8, col = "blue")
text (8.5, 1030, "com evento", cex = 0.8, col = "blue")

#Gráficos Tornado

#DURAÇÃO

matriz_cor_duracao = NULL

for (i in 1:(ncol(cenarios_duracao)-1)) {
  
  if (sd (cenarios_duracao[,i])==0) {
    matriz_cor_duracao[i]=0}
  else {matriz_cor_duracao[i] = cor(cenarios_duracao[,i],duracao_total)}

}


matriz_cor_duracao = matriz_cor_duracao/sum(matriz_cor_duracao)

names (matriz_cor_duracao) = c("INICIO", "A", "B", "C", "D", "E", "F", "G", "FIM")

#preparando tornado plot

o = order(matriz_cor_duracao)

barplot(matriz_cor_duracao[o],beside=TRUE,horiz=TRUE,xlim=c(-1,1), main = "Análise de Sensibilidade - Duração",col = rainbow(9),
        xlab="Coeficientes de correlação normalizados",ylab="Atividade",legend = names(matriz_cor_duracao)[o], yaxt='n')

#CUSTO

matriz_cor_custo = NULL

for (i in 1:(ncol(cenarios_custo)-1)) {
  
  if (sd (cenarios_custo[,i])==0) {
    matriz_cor_custo[i]=0}
  else {matriz_cor_custo[i] = cor(cenarios_custo[,i],custo_total)}
  
}


matriz_cor_custo = matriz_cor_custo/sum(matriz_cor_custo)

names (matriz_cor_custo) = c("INICIO", "A", "B", "C", "D", "E", "F", "G", "FIM")

#preparando tornado plot

o = order(matriz_cor_custo)

barplot(matriz_cor_custo[o],beside=TRUE,horiz=TRUE,xlim=c(-1,1), main = "Análise de Sensibilidade - Custo",col = rainbow(9),
        xlab="Coeficientes de correlação normalizados",ylab="Atividade",legend = names(matriz_cor_custo)[o], yaxt='n')
