taxi2<-function(){
  library (triangle)
  
  #simulando 1 taxis
  Ns<-1000
  r<-vector(length=Ns)
  c<-rtriangle(Ns,40,60,48)
  p<-rtriangle(Ns,3.1,4,3.8)
  g<-c*p
  mg<-mean(g)
  varg<-var(g)
  #média e variancia para 20 carros
  m20<-20*mg
  v20<-20*varg
  d20<-v20^0.5
  r<-list(m20,d20)
  r
}