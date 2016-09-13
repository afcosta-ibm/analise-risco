constSA1<-function(Ns=1000,cont=FALSE,p=0.2){
  #geração de dadocs para análise de sensibilidade
  #modelo tem uma variável constingenciada  
  
  library (triangle)
  
  #run the model  
  Ni<-6
  
  #resulting scenarios matrix
  rs<-matrix(0,nrow=Ns,ncol=Ni+1)
  
  #vetor de eventos
  ev<-vector(length=Ns)
  
  #geração dos cenários
  rs[,1]<-rtriangle(Ns,75,92.5,82.5)
  rs[,2]<-rtriangle(Ns,57.5,77.5,67.5)
  rs[,3]<-rtriangle(Ns,430,472,445)
  rs[,4]<-rtriangle(Ns,140,157.5,145)
  rs[,5]<-rtriangle(Ns,72.5,107.5,92.5)
  rs[,6]<-rtriangle(Ns,80,115,100)
  
  ev<-rbinom(Ns,1,p)
  #Contingenciando
  if (cont){
    rs[,Ni]<-rs[,Ni]*ev}
  
  #totalizando
  for (j in 1:Ni){
    rs[,Ni+1]<-rs[,Ni+1]+rs[,j]
  }
  
  #evaluate correlation coefficients
  c<-cor(rs[,Ni+1],rs[,1:Ni],method="spearman")
  
  #normalization
  c<-c/sum(c)
  m<-matrix(c(1:Ni,c),ncol=2)
  #preparando tornado plot
  o<-order(m[,2])
  yname<-m[,1][o]
  barplot(m[,2][o],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
          names.arg=yname, main = "Cost sensitivity analysis",
          xlab="Normalized correlation coeficient",ylab="Cost item")
  #retorno
  rs
}