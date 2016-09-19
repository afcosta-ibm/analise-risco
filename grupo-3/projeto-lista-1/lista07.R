lista07<-function(){
  library(triangle)
  
  Ns<-3000
  Cenario<-matrix(nrow=Ns,ncol=7)
  Evento<-vector(length=Ns)
  Duracao<-vector(length=Ns)
  
  sortAtividade<-function(){
    atividade<-matrix(nrow=Ns,ncol=7)
    atividade[,1]= rtriangle(Ns, 4,7,5)
    atividade[,2]= rtriangle(Ns, 2,5,2.5)
    atividade[,3]= rtriangle(Ns, 4,6.5,5)
    atividade[,4]= rtriangle(Ns, 1.5, 3,2)
    atividade[,5]= rtriangle(Ns, 4,7,5)
    atividade[,6]= rtriangle(Ns, 2,5,2)
    atividade[,7]= rtriangle(Ns, 1,7,1)
    atividade
  }
  
  sortEvent<-function(){
    tevento<-vector(length=Ns)
    tevento<-rbinom(Ns,1,0.8)
    tevento
  }  
  
  geraDuracao<-function(){
    d<-vector(length=Ns)
    for ( i in 1:Ns){
      if (Evento[i]==1){
        d[i]<-Cenario[i,1]+Cenario[i,2]+ Cenario[i,5]
        
      } else{
        d[i]<-Cenario[i,1]+Cenario[i,3]+Cenario[i,4]+ Cenario[i,5]
      }
      
    }
    d
  }
  
  Cenario<-sortDur()
  Evento<-sortEvent()
  Duracao<-geraDuracao()
  Duracao
}


d<-prazoND()
hist(d)
cd<-ecdf(d)
plot(cd)

