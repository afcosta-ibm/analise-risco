library (triangle)  
library (MASS)

#Quantidade de cenarios
Ns=3000 

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Declaração das matrizes que conterão os cenários @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Matriz de eventos
matEvt<-matrix(nrow=3000,ncol=3)
colnames(matEvt, do.NULL = FALSE)
colnames(matEvt) <- c("PriRet30%","SegRet5%","AdiarPgto50%") 

#sorteios das probabilidades de ocorrencia dos eventos
#Primeiro retrabalho após teste do sistema 
PriRet30 <- rbinom(Ns,1,0.3)

#Segundo retrabalho após teste do sistema 
SegRet5 <- rbinom(Ns,1,0.05)

#Cliente desejar adiar o pagamento da fatura 
AdiarPgto50 <- rbinom(Ns,1,0.5)


for (i in 1:Ns)
{
  matEvt[i,"PriRet30%"] <- PriRet30[i]
  matEvt[i,"SegRet5%"] <- SegRet5[i] 
  matEvt[i,"AdiarPgto50%"] <- AdiarPgto50[i] 
 
}

#Matriz de correlação entre atividades
A=matrix(c(1,0,0,0,0.7,0,0,0,0,0,0,0
            ,0,1,0,0,0,0.7,0,0,0,0,0,0
            ,0,0,1,0,0,0,0.7,0,0,0,0,0
            ,0,0,0,1,0,0,0,0.7,0,0,0,0
            ,0.7,0,0,0,1,0,0,0,0.5,0,0,0
            ,0,0.7,0,0,0,1,0,0,0,0.5,0,0
            ,0,0,0.7,0,0,0,1,0,0,0,0.5,0
            ,0,0,0,0.7,0,0,0,1,0,0,0,0.5
            ,0,0,0,0,0.5,0,0,0,1,0,0,0
            ,0,0,0,0,0,0.5,0,0,0,1,0,0
            ,0,0,0,0,0,0,0.5,0,0,0,1,0
            ,0,0,0,0,0,0,0,0.5,0,0,0,1),ncol=12)
  colnames(A, do.NULL = FALSE)
  colnames(A) <- c("P1","P2","P3","P4","C1","C2","C3","C4","T1","T2","T3","T4")  
  rownames(A, do.NULL = FALSE)
  rownames(A) <- c("P1","P2","P3","P4","C1","C2","C3","C4","T1","T2","T3","T4")  


  #Matriz de prazos
  matPrazo<-matrix(nrow=Ns,ncol=22)
  colnames(matPrazo, do.NULL = FALSE)
  colnames(matPrazo) <- c("PrjSist","P1","P2","P3","P4","C1","C2","C3","C4","T1","T2","T3","T4","PMaiorModulo","Integra","TSist","TAceita","Ret1","TRet1","Ret2","TRet2","Total")  
  
  
  #step 1: generate X[Ns,n] of A-correlated standard normal variates
  m<-rep(0,times=12)
  Z<-mvrnorm(Ns,mu=m,Sigma=A)
  
  #step 2: apply normal cumulative to get correlated uniform variates
  #As uniformes são as probabilidades entre 0 e 1. a correlação é herdada das normais, mas a nuvem é diferente.
  U<-pnorm(Z)

  #step 3:apply inverse cumulative to get triangular variates
  #Calculando os prazos que possuem correlacao
  matPrazo[,"P1"]<-qtriangle(U[,"P1"],2,5,2.5)
  matPrazo[,"P2"]<-qtriangle(U[,"P2"],2,5,2.5)
  matPrazo[,"P3"]<-qtriangle(U[,"P3"],2,5,2.5)
  matPrazo[,"P4"]<-qtriangle(U[,"P4"],2,5,2.5)
  matPrazo[,"C1"]<-qtriangle(U[,"C1"],4,6.5,5)
  matPrazo[,"C2"]<-qtriangle(U[,"C2"],4,6.5,5)
  matPrazo[,"C3"]<-qtriangle(U[,"C3"],4,6.5,5)
  matPrazo[,"C4"]<-qtriangle(U[,"C4"],4,6.5,5)
  matPrazo[,"T1"]<-qtriangle(U[,"T1"],1.5,3,2)
  matPrazo[,"T2"]<-qtriangle(U[,"T2"],1.5,3,2)
  matPrazo[,"T3"]<-qtriangle(U[,"T3"],1.5,3,2)
  matPrazo[,"T4"]<-qtriangle(U[,"T4"],1.5,3,2)
  
  
  #Calculando os prazos nao correlacionados. Os calculos abaixo sao realizados por meio de triangulares simples.
  for (i in 1:Ns)
  {
    matPrazo[i,"PrjSist"]<-rtriangle(1,4,7,5)
    matPrazo[i,"Integra"]<-rtriangle(1,4,7,5)
    matPrazo[i,"TSist"]<-rtriangle(1,2,5,2)
 
    #Prazo maximo devido ao cliente desejar adiar o pagamento foi considerado que
    #o risco deste evento ocorrer e 50% 
    if (matEvt[i,"AdiarPgto50%"]==1)
    {
        matPrazo[i,"TAceita"]<-7
    }
    else
      {
        matPrazo[i,"TAceita"]<-rtriangle(1,1,7,1)
      }

    #Prazo adicional se houver retrabalho
    if (matEvt[i,"PriRet30%"]==1)
    {
        matPrazo[i,"Ret1"]<-rtriangle(1,2,5,3)
        matPrazo[i,"TRet1"]<-rtriangle(1,1,3,1)
        #O segundo retrabalho so ocorrera se tiver havido o primeiro
        if (matEvt[i,"SegRet5%"]==1)
        {
          matPrazo[i,"Ret2"]<-rtriangle(1,1,3,1)
          matPrazo[i,"TRet2"]<-rtriangle(1,1,3,1)
        }
        else
        {
          matPrazo[i,"Ret2"]<-0
          matPrazo[i,"TRet2"]<-0
        }
    }
    else
    {
      # Se o primeiro retrabalho nao tiver ocorrido com 30% de chance, os prazos adicionais sao zero 
      matPrazo[i,"Ret1"]<-0
      matPrazo[i,"TRet1"]<-0
      matPrazo[i,"Ret2"]<-0
      matPrazo[i,"TRet2"]<-0
    }
    
    #Como os modulos podem ser construidos em paralelo, levaremos em consideracao o maior prazo entre eles
    matPrazo[i,"PMaiorModulo"] = matPrazo[i,"P1"] + matPrazo[i,"C1"] + matPrazo[i,"T1"]
    if  (matPrazo[i,"PMaiorModulo"] < (matPrazo[i,"P2"] + matPrazo[i,"C2"] + matPrazo[i,"T2"]))
    {
      matPrazo[i,"PMaiorModulo"] <- (matPrazo[i,"P2"] + matPrazo[i,"C2"] + matPrazo[i,"T2"])
    }
    if  (matPrazo[i,"PMaiorModulo"] < (matPrazo[i,"P3"] + matPrazo[i,"C3"] + matPrazo[i,"T3"]))
    {
      matPrazo[i,"PMaiorModulo"] <- (matPrazo[i,"P3"] + matPrazo[i,"C3"] + matPrazo[i,"T3"])
    }
    if  (matPrazo[i,"PMaiorModulo"] < (matPrazo[i,"P4"] + matPrazo[i,"C4"] + matPrazo[i,"T4"]))
    {
      matPrazo[i,"PMaiorModulo"] <- (matPrazo[i,"P4"] + matPrazo[i,"C4"] + matPrazo[i,"T4"])
    }
    
    matPrazo[i,"Total"] <- matPrazo[i,"PrjSist"] + matPrazo[i,"PMaiorModulo"] + matPrazo[i,"Integra"] + matPrazo[i,"TSist"] + matPrazo[i,"TAceita"] + matPrazo[i,"Ret1"] + matPrazo[i,"TRet1"] + matPrazo[i,"Ret2"] + matPrazo[i,"TRet2"]

    }

hist(matPrazo[,"Total"])
cum <- ecdf(matPrazo[,"Total"])
plot(cum)  
abline(v = mean(matPrazo[,"Total"]), col = "red") 
abline(h=0.85, col = "blue") 
perc<-seq(from=0,to=1,by=0.05)
quantile(matPrazo[,"Total"],perc)
plot(matPrazo[,"PrjSist"],matPrazo[,"Total"])
abline(lm(matPrazo[,"Total"]~matPrazo[,"PrjSist"]),col="red")
