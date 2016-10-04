#Aluno:Leonardo Mendonça Pires

#@@@@@@@Informações sobre a organização do programa@@@@@@@@@
#1) Os cenários estão separados nas seguintes matrizes
#2) matCusto = Matriz com os cenarios de custos
#3) matPrazo = Matriz com os cenários de prazo
#4) matEvt = Matriz com os cenários de eventos
#5) A = Matriz de correlação
#6) matResultado = Matriz que possui duas colunas, uma com o total do prazo
#e outra com o total do custo
#7) No final do programa existe uma seção denominada "Exibindo os resultados",
# lá estão as respostas para as perguntas da Prova


library(igraph)
library(triangle)
library (MASS)

#Quantidade de cenários que iremos utilizar na soluição do problema
ns=3000

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Declaração das matrizes que conterão os cenários @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Matriz que armazenará o resultado dos cálculos de Prazo e custo
matResultado<-matrix(nrow=ns,ncol=2)
colnames(matResultado, do.NULL = FALSE)
colnames(matResultado) <- c("TotalPrazo","TotalCusto")
#-------------------------------------------------------------------------

#Matriz de correlação entre atividades
A=matrix(c(1,0,0,0,0,0,0,
           0,1,0,0.4,0,0,0,
           0,0,1,0,0,0,0,
           0,0.4,0,1,0,0,0,
           0,0,0,0,1,0.7,0,
           0,0,0,0,0.7,1,0,
           0,0,0,0,0,0,1),ncol=7)
colnames(A, do.NULL = FALSE)
colnames(A) <- c("A","B","C","D","E","F","G")  
rownames(A, do.NULL = FALSE)
rownames(A) <- c("A","B","C","D","E","F","G")  
#--------------------------------------------------------------------------

#Montando a Matriz de eventos 
matEvt<-matrix(nrow=ns,ncol=1)
colnames(matEvt, do.NULL = FALSE)
colnames(matEvt) <- c("F25%")

#sorteios das probabilidades de ocorrencia dos eventos
#25% de chances de F ser executado
F25 <- rbinom(ns,1,0.25)

for (i in 1:ns)
{
  matEvt[i,"F25%"] <- F25[i]
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Início do cálculo dos prazos para carregar a matriz matPrazo @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##step 1:generate A-correlated standard normal variates
m<-rep(0,times=7)
Z<-mvrnorm(ns,mu=m,Sigma=A)

#step 2: apply normal cumulative to get correlated uniform variates
#As uniformes são as probabilidades entre 0 e 1. a correlação é herdada das normais, mas a nuvem é diferente.
U<-pnorm(Z)

#Matriz de prazos
matPrazo<-matrix(nrow=ns,ncol=8)
colnames(matPrazo, do.NULL = FALSE)
colnames(matPrazo) <- c("A","B","C","D","E","F","G","Total")  

#step 3:apply inverse cumulative to get triangular variates
#Calculando os prazos que possuem correlacao
matPrazo[,"B"]<-qtriangle(U[,"B"],5,7,6)
matPrazo[,"D"]<-qtriangle(U[,"D"],1,4,3)
matPrazo[,"E"]<-qtriangle(U[,"E"],4,7,5)
matPrazo[,"F"]<-qtriangle(U[,"F"],3,5,4)
#Calculando os prazos nao correlacionados. Os calculos abaixo sao realizados por meio de triangulares simples.
for (i in 1:ns)
{
  matPrazo[i,"A"]<-rtriangle(1,1,4,2)
  matPrazo[i,"C"]<-rtriangle(1,2,5,4)
  matPrazo[i,"G"]<-rtriangle(1,1,3,2)
 
 }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Início do cálculo dos custos para carregar a matriz matCusto @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Matriz de custos
matCusto<-matrix(nrow=ns,ncol=8)
colnames(matCusto, do.NULL = FALSE)
colnames(matCusto) <- c("A","B","C","D","E","F","G","Total")  

#step 3:apply inverse cumulative to get triangular variates
#Calculando os prazos que possuem correlacao
matCusto[,"B"]<-qtriangle(U[,"B"],120,190,150)
matCusto[,"D"]<-qtriangle(U[,"D"],45,80,60)
matCusto[,"E"]<-qtriangle(U[,"E"],160,240,200)
matCusto[,"F"]<-qtriangle(U[,"F"],210,300,250)
#Calculando os custos nao correlacionados. Os calculos abaixo sao realizados por meio de triangulares simples.
for (i in 1:ns)
{
  matCusto[i,"A"]<-rtriangle(1,75,110,90)
  matCusto[i,"C"]<-rtriangle(1,60,85,75)
  matCusto[i,"G"]<-rtriangle(1,85,140,120)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#MONTANDO O GRAFO @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
elosOriginais <- c(1,2,1,3,1,4,2,5,2,7,3,8,4,6,5,8,6,8,7,9,8,9)

#Elos que sempre existirão
elos <- c(1,2,1,3,1,4,2,5,3,8,4,6,5,8,6,8,8,9)

#Elo F incluído com probabilidade de 25%
eloF25 <- c(2,7,7,9)

todosElos<-vector(length=ns)

#Iremos agora gerar os cenários, levando-se em consideração a possível alteração do GRAFO
#devido a ocorrência do evento da Atividade F
for (i in 1:ns)
{
    #Elos que sempre existirão
    elos <- c(1,2,1,3,1,4,2,5,3,8,4,6,5,8,6,8,8,9)
  
    #Incluindo os elos se o evento ocorrer
    if (matEvt[i,"F25%"]==1)
    {
      elos <- c(elos,eloF25)
    } 
#}

#Montando o grafo com os elos
g<-make_graph(elos,n=9)
#plot(g)
#Encontrando todos os caminhos
p<-all_simple_paths(g,from = 1,to=9)


#Obtendo o tamanho do grafo
t<-length(p)

#Iremos agora percorrer os cenários

#Cálculo do Prazo Total - Percorrendo os cenários e utilizando o grafo para encontrar o maior caminho  
 d<- c(0,matPrazo[i,"A"],matPrazo[i,"B"],matPrazo[i,"C"],matPrazo[i,"D"],matPrazo[i,"E"],matPrazo[i,"F"],matPrazo[i,"G"],0)
 maxd<-0
 for (i2 in 1:t){
   pd<-sum(d[p[[i2]]])
   if (pd > maxd) {
     maxd<-pd
   }  
 }
  matPrazo[i,"Total"] <- maxd

 #Cálculo do Custo Total - Percorrendo os cenários e utilizando o grafo para encontrar o maior caminho
 d<- c(0,matCusto[i,"A"],matCusto[i,"B"],matCusto[i,"C"],matCusto[i,"D"],matCusto[i,"E"],matCusto[i,"F"],matCusto[i,"G"],0)
 maxd<-0
 for (i2 in 1:t){
   pd<-sum(d[p[[i2]]])
   if (pd > maxd) {
     maxd<-pd
   }  
 }
 matCusto[i,"Total"] <- maxd

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Carregando a Matriz de resultados
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
matResultado[,"TotalPrazo"] <-  matPrazo[,"Total"]
matResultado[,"TotalCusto"] <- matCusto[,"Total"]


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Exibindo os resultados
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# >> Questão 2
#Plotando o GRAFO
plot(g)



# >> Questão 3 
#Exibindo as probabilidades do prazo
hist(matResultado[,"TotalPrazo"])
cum <- ecdf(matResultado[,"TotalPrazo"])
plot(cum)  
abline(v = mean(matResultado[,"TotalPrazo"]), col = "red") 
abline(h=0.85, col = "blue") 
perc<-seq(from=0,to=1,by=0.05)
quantile(matResultado[,"TotalPrazo"],perc)
# 0%        5%       10%       15%       20%       25%       30%       35%       40%       45%       50% 
#7.844530  9.378866  9.733699  9.949657 10.142764 10.319543 10.460477 10.597022 10.733951 10.867330 10.995973 
# 55%       60%       65%       70%       75%       80%       85%       90%       95%      100% 
#11.117321 11.220704 11.341755 11.487075 11.650631 11.823171 12.045821 12.324653 12.657509 13.968419 
#@@@@@@@ Em 85% dos casos o prazo ficará abaixo de 12.045821, que em muitos casos representa uma boa 
#        margem de risco para tomada de decisões   



# >> Questão 4
#Exibindo as probabilidades do custo
hist(matResultado[,"TotalCusto"])
cum <- ecdf(matResultado[,"TotalCusto"])
plot(cum)  
abline(v = mean(matResultado[,"TotalCusto"]), col = "red") 
abline(h=0.85, col = "blue") 
perc<-seq(from=0,to=1,by=0.05)
quantile(matResultado[,"TotalCusto"],perc)
#%       5%      10%      15%      20%      25%      30%      35%      40%      45%      50%      55%      60% 
#324.8121 353.5291 361.6663 366.2652 370.2369 373.5381 376.7836 379.6926 382.3858 385.1382 387.7958 390.7458 393.8600 
#65%      70%      75%      80%      85%      90%      95%     100% 
#396.5310 399.1322 402.9955 406.6052 410.4110 415.2173 422.6100 448.4631 
#@@@@@@@ Em 85% dos casos o custo ficará abaixo de 410.4110, que em muitos casos representa uma boa 
#        margem de risco para tomada de decisões  


# >> Questão 5 
plot(matResultado[,"TotalPrazo"],matResultado[,"TotalCusto"])
abline(lm(matResultado[,"TotalCusto"]~matResultado[,"TotalPrazo"]),col="red")
cor(matResultado[,"TotalCusto"],matResultado[,"TotalPrazo"])
#pairs(matResultado[,"TotalCusto"],matResultado[,"TotalPrazo"])



# >> Questão 6
#Gráfico Tornado do Prazo 
c<-cor(matPrazo[,"Total"],matPrazo[,1:7],method="spearman")
#c3<-c(1:7,c)
#normalization
c<-c/sum(c)
m<-matrix(c(1:7,c),ncol=2)
#preparando TORNADO plot
o<-order(m[,2])
yname<-m[,1][o]
barplot(m[,2][o],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
        names.arg=yname, main = "Gráfico Tornado do Prazo",
        xlab="Normalized correlation coeficient",ylab="Cost item")

#Géafico Tornado do Custo 
c<-cor(matCusto[,"Total"],matCusto[,1:7],method="spearman")
#c3<-c(1:7,c)
#normalization
c<-c/sum(c)
m<-matrix(c(1:7,c),ncol=2)
#preparando TORNADO plot
o<-order(m[,2])
yname<-m[,1][o]
barplot(m[,2][o],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
        names.arg=yname, main = "Gráfico Tornado do Custo",
        xlab="Normalized correlation coeficient",ylab="Cost item")
