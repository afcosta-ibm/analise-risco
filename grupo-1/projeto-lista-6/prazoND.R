prazoND<-function(){
  library(triangle)
  
  Ns<-3000
  Cenario<-matrix(nrow=Ns,ncol=5)
  Evento<-vector(length=Ns)
  Duracao<-vector(length=Ns)
  
  sortDur<-function(){
    tCenario<-matrix(nrow=Ns,ncol=5)
    tCenario[,1]<-rtriangle(Ns,2,5,4)
    tCenario[,2]<-rtriangle(Ns,3,7,5)
    tCenario[,3]<-rtriangle(Ns,4,9,5)
    tCenario[,4]<-rtriangle(Ns,2,4,3)
    tCenario[,5]<-rtriangle(Ns,5,10,6)
    tCenario
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