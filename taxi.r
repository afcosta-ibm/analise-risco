taxi<-function(){
  library (triangle)
  
  #simulando 20 taxis
  Ns<-1000
  r<-vector(length=Ns)
  for (i in 1:Ns){
    #vetor de consumos
    c<-rtriangle(20,40,60,48)
    p<-rtriangle(20,3.1,4,3.8)
    g<-sum(c*p)
    r[i]<-g
  }
  r
}