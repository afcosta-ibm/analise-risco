construcao<-function(Ns=1000){
  library (MASS)
  library (triangle)
  
  Ni<-5
  rs<-vector(length=Ns)
  rm<-vector(length=Ns)
  dm<-vector(length=Ns)
  pt=matrix(data=c(75,92.5,82.5,
                   57.5,77.5,67.5,
                   430,472.5,445,
                   140,157.5,145,
                   72.5,107.5,92.5),
            nrow=Ni,ncol=3,byrow=TRUE)
  
  for (i in 1:Ns){
    #generate on scenario
    rs[i]<-0
    rm[i]<-0
    dm[1]<-0
    for (j in 1:Ni){
      rs[i]<-rs[i]+rtriangle(1,pt[j,1],pt[j,2],pt[j,3])
      rm[i]<-mean(rs[1:i])
      if (i!= 1){
        dm[i]<-rm[i]-rm[i-1]
      }
    }
  }
  #r<-list(rs,rm,dm)
  rs
  
}


