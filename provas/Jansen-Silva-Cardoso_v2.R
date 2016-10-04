Prova <- function()
{
library (triangle)  
library (MASS)

#Quantidade de cenarios
Ns=3000 

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



#Matriz de prazos
matPrazo<-matrix(nrow=Ns,ncol=11)
colnames(matPrazo, do.NULL = FALSE)
colnames(matPrazo) <- c("INICIO","A","B","C","D","E","F","G","EVENTOF","FIM","PRAZOTOTAL")  

#Matriz de custos
matcusto<-matrix(nrow=Ns,ncol=11)
colnames(matcusto, do.NULL = FALSE)
colnames(matcusto) <- c("INICIO","A","B","C","D","E","F","G","EVENTOF","FIM","CUSTOTOTAL") 


#Cria correlação
m<-rep(0,times=7)
Z<-mvrnorm(Ns,mu=m,Sigma=A)
U<-pnorm(Z)

#Popula as matrizes com os dados para prazo
matPrazo[,"A"]<-qtriangle(U[,"A"],1,4,2)
matPrazo[,"B"]<-qtriangle(U[,"B"],5,7,6)
matPrazo[,"C"]<-qtriangle(U[,"C"],2,5,4)
matPrazo[,"D"]<-qtriangle(U[,"D"],1,4,3)
matPrazo[,"E"]<-qtriangle(U[,"E"],4,7,5)
matPrazo[,"F"]<-qtriangle(U[,"F"],3,5,4)
matPrazo[,"G"]<-qtriangle(U[,"G"],1,3,2)
matPrazo[,"INICIO"]<-0 # Dummy Início
matPrazo[,"FIM"]<-0 # Dummy Fim
matPrazo[,"EVENTOF"]<- rbinom(Ns,1,0.25)

#Popula as matrizes com os dados para custo
matcusto[,"A"]<-qtriangle(U[,"A"],75,110,90)
matcusto[,"B"]<-qtriangle(U[,"B"],120,190,150)
matcusto[,"C"]<-qtriangle(U[,"C"],60,85,75)
matcusto[,"D"]<-qtriangle(U[,"D"],45,80,60)
matcusto[,"E"]<-qtriangle(U[,"E"],160,240,200)
matcusto[,"F"]<-qtriangle(U[,"F"],210,300,250)
matcusto[,"G"]<-qtriangle(U[,"G"],85,140,120)
matcusto[,"INICIO"]<-0 # Dummy Início
matcusto[,"FIM"]<-0 # Dummy Fim
matcusto[,"EVENTOF"]<- matPrazo[,"EVENTOF"]

#Construção do grafo
library(igraph)
g <- c(1,2,1,3,1,4,2,8,3,5,3,6,4,7,5,8,7,8,6,9,8,9)
grafo <- make_graph(g) 
plot(grafo) #Gráfico do grafo

#Caminho possíveis
path=all_simple_paths(grafo,from=1,to=9)
list(path)

#Total de caminhos possíveis
Caminho1 <-vector(length=Ns)
Caminho2 <-vector(length=Ns)
Caminho3 <-vector(length=Ns)
Caminho4 <-vector(length=Ns)

#Calculando o maior caminho dos grafos e colocando o total para prazo e custo
   for (i in 1:Ns){
      Caminho1<-matPrazo[i,"INICIO"]+matPrazo[i,"B"]+matPrazo[i,"G"]+matPrazo[i,"FIM"]
      Caminho2<-matPrazo[i,"INICIO"]+matPrazo[i,"A"]+matPrazo[i,"D"]+matPrazo[i,"G"]+matPrazo[i,"FIM"]
      if (matPrazo[Ns,"EVENTOF"] == 1)
      {
         Caminho3<-matPrazo[i,"INICIO"]+matPrazo[i,"A"]+matPrazo[i,"F"]+matPrazo[i,"FIM"]  
      }
      else
      {
         Caminho3<-0
      }
      Caminho4<-matPrazo[i,"INICIO"]+matPrazo[i,"C"]+matPrazo[i,"E"]+matPrazo[i,"G"]+matPrazo[i,"FIM"]
      matPrazo[i,"PRAZOTOTAL"]<-max(Caminho1,Caminho2,Caminho3,Caminho4)
      if (matPrazo[i,"PRAZOTOTAL"] == Caminho1)
      {
         matcusto[i,"CUSTOTOTAL"]<-matcusto[i,"INICIO"]+matcusto[i,"B"]+matcusto[i,"G"]+matcusto[i,"FIM"]
      }
      if (matPrazo[i,"PRAZOTOTAL"] == Caminho2)
      {
         matcusto[i,"CUSTOTOTAL"]<-matcusto[i,"INICIO"]+matcusto[i,"A"]+matcusto[i,"D"]+matcusto[i,"G"]+matcusto[i,"FIM"]
      }
      if (matPrazo[i,"PRAZOTOTAL"] == Caminho3)
      {
         matcusto[i,"CUSTOTOTAL"]<-matcusto[i,"INICIO"]+matcusto[i,"A"]+matcusto[i,"F"]+matcusto[i,"FIM"]  
      }
      if (matPrazo[i,"PRAZOTOTAL"] == Caminho4)
      {
         matcusto[i,"CUSTOTOTAL"]<-matcusto[i,"INICIO"]+matcusto[i,"C"]+matcusto[i,"E"]+matcusto[i,"G"]+matcusto[i,"FIM"]
      }
   }
 

hist(matPrazo[,"PRAZOTOTAL"]) #Distribuição de probabilidade do prazo
hist(matcusto[,"CUSTOTOTAL"]) #Distribuição de probabilidade do custo
plot(ecdf(matPrazo[,"PRAZOTOTAL"])) #Distribuição cumulativa do prazo
plot(ecdf(matcusto[,"CUSTOTOTAL"])) #Distribuição cumulativa do custo


# Criação do gráfico tornado para prazo
c<-cor(matPrazo[,11],matPrazo[,2:8],method="spearman")
m<-matrix(c(2:8,c),ncol=2)
o<-order(m[,2])
yname<-m[,1][o]
barplot(m[,2][o],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
        names.arg=yname, main = "Cost sensitivity analysis",
        xlab="Normalized correlation coeficient",ylab="Cost item")

# Criação do gráfico tornado para custo
c<-cor(matcusto[,11],matcusto[,2:8],method="spearman")
m<-matrix(c(2:8,c),ncol=2)
o<-order(m[,2])
yname<-m[,1][o]
barplot(m[,2][o],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
        names.arg=yname, main = "Cost sensitivity analysis",
        xlab="Normalized correlation coeficient",ylab="Cost item")
}