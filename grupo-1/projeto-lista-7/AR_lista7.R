#Lista 7 - Risco de prazo com correlação

library(triangle)
library (MASS)
library(igraph)

# GRAFO

#create graph
g = make_graph(c(1,2,1,3,1,4,1,5,2,6,3,7,4,8,5,9,6,10,7,11,8,12,9,13,10,14,11,14,12,
                 14,13,14,14,15,15,16,16,17,16,21,17,18,18,19,18,21,19,20,20,21),directed=FALSE)
plot(g, main = "Grafo Representante")

#generate all paths
path = all_simple_paths(g,from=1,to=21)
print(path)

#find longesth path
l = length(path)

#PROJETO DE SISTEMA

projeto.sistema = function () {
  prazo_projetosistema = rtriangle (N,4,7,5)
  return (prazo_projetosistema)
}

#Existe uma correlação de 70% entre o esforço de projeto de um módulo e a sua
#codificação, assim como uma correlação de 50% entre a codificação e teste de um módulo.

#GERAR TRIANGULAR MULTIVARIADA
tri.multi = function (C, N) {
  
  #gerar Z[m,n], m n-plas amostras normais reduzidas correlacionadas por C:
  
  Z<-mvrnorm(N,mu=rep(0,times=3),Sigma=C)
  
  #gerar U com m n-plas uniformes:
  
  U<-pnorm(Z)
  
  return (U)
}

#PROJETO DE MÓDULO 1 (x4)

projeto.modulo = function (U) {
  prazo_projetomodulo = qtriangle (U[,1],2,5,2.5)
  return (prazo_projetomodulo)
}

#CODIFICAÇÃO DO MÓDULO (x4)

codificacao.modulo = function (U) {
  prazo_codificacaomodulo = qtriangle (U[,2],4,6.5,5)
  return (prazo_codificacaomodulo)
}

#TESTE DE MÓDULO (x4)

teste.modulo = function (U) {
  prazo_testemodulo = qtriangle (U[,3],1.5,3,2)
  return (prazo_testemodulo)
}

#INTEGRAÇÃO

integracao = function () {
  prazo_integracao = rtriangle (N,4,7,5)
  return (prazo_integracao)
}

#TESTE DO SISTEMA SEM RETRABALHO

teste.sistema = function () {
  prazo_testesistema = rtriangle (N,2,5,2)
  return (prazo_testesistema)
}

#TESTE DE ACEITAÇÃO

teste.aceitacao = function () {
  prazo_testeaceitacao = rtriangle (N,1,7,1)
  return (prazo_testeaceitacao)
}

#RETRABALHO 1

retrabalho1 = function () {
  chance_retrabalho1 = rbinom (1,1,0.3)
  if (chance_retrabalho1 == 1) {
    prazo_retrabalho1 = rtriangle (1,2,5,3) }
  else {prazo_retrabalho1 = 0}
  
  return (c(prazo_retrabalho1, chance_retrabalho1))
}

#RETRABALHO 2

retrabalho2 = function (ret1) {
  if (ret1 == 1) {
    chance_retrabalho2 = rbinom (1,1,0.05)}
  else {chance_retrabalho2 = 0}
  
  if (chance_retrabalho2 == 1) {
    prazo_retrabalho2 = rtriangle (1,1,3,1) }
  else {prazo_retrabalho2 = 0}
  
  return (c(prazo_retrabalho2, chance_retrabalho2))
}

#TESTE DO SISTEMA APÓS RETRABALHO

teste.retrabalho = function () {
  prazo_testeretrabalho = rtriangle(1,1,7,1)
  return (prazo_testeretrabalho)
}

#SIMULAÇÃO

N = 3000
C = matrix (data = c(1,0.7,0,0.7,1,0.5,0,0.5,1), ncol=3)

cenarios = matrix(nrow = N, ncol = 23)
colnames (cenarios) = c("proj_sistema", "proj_mod1", "proj_mod2", "proj_mod3",
                        "proj_mod4", "codif_mod1", "codif_mod2", "codif_mod3",
                        "codif_mod4", "teste_mod1", "teste_mod2", "teste_mod3",
                        "teste_mod4", "integracao", "teste_sis", "teste_aceit", 
                        "ret1", "teste_ret1", "ret2", "teste_ret2", "fim",
                        "evento_ret1", "evento_ret2")

  cenarios[ ,1] = projeto.sistema()
  U1 = tri.multi(C,N)
  U2 = tri.multi(C,N)
  U3 = tri.multi(C,N)
  U4 = tri.multi(C,N)
  cenarios[ ,2] = projeto.modulo(U1)
  cenarios[ ,3] = projeto.modulo(U2)
  cenarios[ ,4] = projeto.modulo(U3)
  cenarios[ ,5] = projeto.modulo(U4)
  cenarios[ ,6] = codificacao.modulo(U1)
  cenarios[ ,7] = codificacao.modulo(U2)
  cenarios[ ,8] = codificacao.modulo(U3)
  cenarios[ ,9] = codificacao.modulo(U4)
  cenarios[ ,10] = teste.modulo(U1)
  cenarios[ ,11] = teste.modulo(U2)
  cenarios[ ,12] = teste.modulo(U3)
  cenarios[ ,13] = teste.modulo(U4)
  cenarios[ ,14] = integracao()
  cenarios[ ,15] = teste.sistema() #teste sem retrabalho
  cenarios[ ,16] = teste.aceitacao()
  
for (i in 1:N) {    
  ret1 = retrabalho1()
  cenarios[i,17] = ret1[1] #retrabalho 1
  
  if (ret1[2] == 1){
    cenarios[i,18] = teste.retrabalho() #teste após retrabalho 1
  } else {cenarios[i,18] = 0}
  
  ret2 = retrabalho2(ret1[2])
  cenarios[i,19] = ret2[1] #retrabalho 2
  
  if (ret2[2] == 1) {
    cenarios[i,20] = teste.retrabalho() #teste após retrabalho 2
  } else {cenarios[i,20] = 0}
  
  #fim
  cenarios [i,21] = 0
  
  #eventos
  cenarios[i,22] = ret1[2] #chance de retrabalho 1
  cenarios[i,23] = ret2[2] #chance de retrabalho 2
}

#PRAZO TOTAL DE ENTREGA  

prazo_total = NULL
  
# for (j in 1:N) {
#  prazo_total[j] = cenarios[j,1] + sum(cenarios[j,14:20])+
#                   max(cenarios[j,2]+cenarios[j,6]+cenarios[j,10], 
#                       cenarios[j,3]+cenarios[j,7]+cenarios[j,11], 
#                       cenarios[j,4]+cenarios[j,8]+cenarios[j,12], 
#                       cenarios[j,5]+cenarios[j,9]+cenarios[j,13])
# }

for (i in 1:N) {
  #generate activity duration vectors
  d = cenarios[i,]
  maxd = 0
  for (j in 1:l){
    pd = sum(d[path[[j]]])
    if (pd > maxd) {
      maxd = pd
    }	
  }	
  prazo_total[i] = maxd
}

# same=0
# for (i in 1:N) {
#   if(prazos[i]==prazo_total[i]) {same=same+1}
# }

  
hist(prazo_total, main = "Histograma do prazo de entrega do sistema")
mean(prazo_total)
sd(prazo_total)

# CONFERINDO AS CORRELAÇÕES
cor(cenarios[,2],cenarios[,6])
#[1] 0.6944246
cor(cenarios[,2],cenarios[,10])
#[1] 0.01302156
cor(cenarios[,6],cenarios[,10])
#[1] 0.4997756

#Gráfico de percentis
perc = (seq(0, 1, by=0.01))

x = quantile (prazo_total, probs=perc)

plot(x, perc, type = 'l', col = "red", 
     main = "", xlab = "prazo total", ylab = "percentil",
     xlim = c(min(x), max(x)))

d = matrix(nrow = length(perc), ncol = 2 )
d[,1] = x
d[,2] = perc

#Riscos de prazo: d[86,1] (85%) e d[71,1] (71%)

d[86,1]
d[71,1]

abline (v = d[86,1], col = "gray")
abline (h = d[86,2], col = "gray")
abline (v = d[71,1], col = "gray")
abline (h = d[71,2], col = "gray")
points (d[86,1], d[86,2], col = "green", cex = 0.9)
points (d[71,1], d[71,2], col = "green", cex = 0.9)
text (d[86,1], d[90,2], "p:85%, prazo total:34.9", cex = 1.2, col = "blue")
text (d[71,1], d[75,2], "p:70%, prazo total:31.9", cex = 1.2, col = "blue")

#data de entrega: entre 13/08 e 03/09 de 2017.


