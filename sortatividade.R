#Lista 07
lista<-function(){
  amostras = 3000
  library(triangle)
  
  sortCenarios <- function(){
  
    matriz = matrix(nrow=amostras,ncol=20)  
    matriz[,1] <- rtriangle(amostras, 4, 7, 5)
    matriz[,2] <- rtriangle(amostras, 2, 5, 2.5)
    matriz[,3] <- rtriangle(amostras, 2, 5, 2.5)
    matriz[,4] <- rtriangle(amostras, 2, 5, 2.5)
    matriz[,5] <- rtriangle(amostras, 2, 5, 2.5)
    matriz[,6] <- rtriangle(amostras, 4, 6.5, 5)
    matriz[,7] <- rtriangle(amostras, 4, 6.5, 5)
    matriz[,8] <- rtriangle(amostras, 4, 6.5, 5)
    matriz[,9] <- rtriangle(amostras, 4, 6.5, 5)
    matriz[,10] <- rtriangle(amostras, 1.5, 3, 2)
    matriz[,11] <- rtriangle(amostras, 1.5, 3, 2)
    matriz[,12] <- rtriangle(amostras, 1.5, 3, 2)
    matriz[,13] <- rtriangle(amostras, 1.5, 3, 2)
    matriz[,14] <- rtriangle(amostras, 4, 7, 5)
    matriz[,15] <- rtriangle(amostras, 2, 5, 2)
    matriz[,16] <- rtriangle(amostras, 1, 7, 1)
    matriz[,17] <- rtriangle(amostras, 2, 5, 3)
    matriz[,18] <- rtriangle(amostras, 1, 3, 1)
    matriz[,19] <- rtriangle(amostras, 1, 3, 1)
    matriz[,20] <- rtriangle(amostras, 1, 3, 1)
    matriz
  }
  
  gen2CorrTriangsMv<-function(Ns=3000,A=matrix(c(0.7,0.3,0.3,0.7),ncol=2)){        
    library (triangle)  
    library (MASS)
    #Algorithm
    #Inputs: n:number of variables, Ns: number of samples,  A: corr matrix 
    #Output: T[Ns,n] of correlated triang variates
    
    #step 1: generate X[Ns,n] of A-correlated standard normal variates
    m<-rep(0,times=2)
    Z<-mvrnorm(Ns,mu=m,Sigma=A)
    
    plot(Z[,1],Z[,2]) 
    #step 2: apply normal cumulative to get correlated uniform variates
    U<-pnorm(Z)
    plot(U[,1],U[,2])
    
    #step 3:apply inverse cumulative to get triangular variates
    Tri<-matrix(nrow=Ns,ncol=2)
    Tri[,1]<-qtriangle(U[,1],2,5,2.5)#projeto
    Tri[,2]<-qtriangle(U[,2],4,6.5,5)#codificacao
    
    plot(Tri[,1],Tri[,2])
    
    Tri
  }
  Tri = gen2CorrTriangsMv()
  z = sortCenarios()
  }